{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Header
  ( Info(..)
  , readModule
  , readOneFile
  , readManyFiles
  , readSource
  )
  where

import Control.Monad.Except (liftIO)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Time.Calendar as Day
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module

import qualified Elm.Project.Json as Project
import Elm.Project.Json (Project)
import Elm.Project.Summary (Summary(..))
import qualified File.IO as IO
import qualified Reporting.Error as Error
import qualified Reporting.Error.Crawl as E
import qualified Reporting.Task as Task



-- INFO


data Info =
  Info
    { _path :: FilePath
    , _time :: Time.UTCTime
    , _source :: Text
    , _imports :: [Module.Raw]
    }



-- CRAWL TASK


type Task a =
  Task.Task_ E.Error a


atRoot :: Task a -> Task.Task a
atRoot task =
  Task.mapError Error.BadCrawlRoot task



-- READ MODULE


readModule :: Summary -> Module.Raw -> FilePath -> Task (Module.Raw, Info)
readModule summary expectedName path =
  do  time <- liftIO $ Dir.getModificationTime path
      source <- liftIO $ IO.readUtf8 path
      (maybeName, info) <- parse (_project summary) path time source
      name <- checkName path expectedName maybeName
      return (name, info)



-- READ ONE FILE


readOneFile :: Summary -> FilePath -> Task.Task (Maybe Module.Raw, Info)
readOneFile summary path =
  do  time <- liftIO $ Dir.getModificationTime path
      source <- liftIO $ IO.readUtf8 path
      atRoot $ parse (_project summary) path time source



-- READ MANY FILES


readManyFiles :: Summary -> NonEmpty FilePath -> Task.Task (NonEmpty (Module.Raw, Info))
readManyFiles summary files =
  traverse (readManyFilesHelp summary) files


readManyFilesHelp :: Summary -> FilePath -> Task.Task (Module.Raw, Info)
readManyFilesHelp summary path =
  do  time <- liftIO $ Dir.getModificationTime path
      source <- liftIO $ IO.readUtf8 path
      (maybeName, info) <- atRoot $ parse (_project summary) path time source
      case maybeName of
        Nothing ->
          error ("TODO module at " ++ path ++ " needs a name")

        Just name ->
          return (name, info)



-- READ SOURCE


readSource :: Project -> Text -> Task.Task (Maybe Module.Raw, Info)
readSource project source =
  atRoot (parse project "elm" fakeTime source)


fakeTime :: Time.UTCTime
fakeTime =
  Time.UTCTime (Day.fromGregorian 2990 2 2) 0



-- PARSE HEADER


parse :: Project -> FilePath -> Time.UTCTime -> Text -> Task (Maybe Module.Raw, Info)
parse project path time source =
  -- TODO get regions on data extracted here
  case Compiler.parseHeader (Project.getName project) source of
    Right (tag, maybeName, deps) ->
      do  checkTag project path tag
          return ( maybeName, Info path time source deps )

    Left msg ->
      Task.throw (E.BadHeader path msg)



-- CHECKS


checkName :: FilePath -> Module.Raw -> Maybe Module.Raw -> Task Module.Raw
checkName path expectedName maybeName =
  case maybeName of
    Nothing ->
      Task.throw (E.NoName path expectedName)

    Just actualName ->
      if expectedName == actualName
        then return expectedName
        else Task.throw (E.BadName path actualName)


checkTag :: Project -> FilePath -> Compiler.Tag -> Task ()
checkTag project path tag =
  case tag of
    Compiler.Normal ->
      return ()

    Compiler.Port ->
      case project of
        Project.App _ ->
          return ()

        Project.Pkg _ ->
          Task.throw (E.PortsInPackage path)

    Compiler.Effect ->
      if Project.getEffect project then
        return ()

      else
        Task.throw (E.EffectsUnexpected path)

