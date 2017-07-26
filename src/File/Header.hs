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
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import qualified Data.Text as Text
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
    , _source :: Text.Text
    , _imports :: [Module.Raw]
    }


atRoot :: Task.Task_ E.Problem a -> Task.Task_ E.Error a
atRoot task =
  Task.mapError (\problem -> E.DependencyProblems problem []) task


-- READ MODULE


readModule :: Summary -> Module.Raw -> FilePath -> Task.Task_ E.Problem (Module.Raw, Info)
readModule summary expectedName path =
  do  time <- liftIO $ Dir.getModificationTime path
      source <- liftIO $ IO.readUtf8 path
      (maybeName, info) <- parse (_project summary) path time source
      name <- checkName path expectedName maybeName
      return (name, info)


checkName :: FilePath -> Module.Raw -> Maybe Module.Raw -> Task.Task_ E.Problem Module.Raw
checkName path expectedName maybeName =
  case maybeName of
    Nothing ->
      Task.throw (E.ModuleNameMissing path expectedName)

    Just actualName ->
      if expectedName == actualName
        then return expectedName
        else Task.throw (E.ModuleNameMismatch path expectedName actualName)



-- READ ONE FILE


readOneFile :: Summary -> FilePath -> Task.Task (Maybe Module.Raw, Info)
readOneFile summary path =
  Task.mapError Error.Crawl $
  do  time <- liftIO $ Dir.getModificationTime path
      source <- liftIO $ IO.readUtf8 path
      atRoot $ parse (_project summary) path time source



-- READ MANY FILES


readManyFiles :: Summary -> NonEmpty FilePath -> Task.Task (NonEmpty (Module.Raw, Info))
readManyFiles summary files =
  Task.mapError Error.Crawl $
  do  infos <- traverse (readManyFilesHelp summary) files
      let insert (name, info) dict = Map.insertWith (<>) name (info :| []) dict
      let nameTable = foldr insert Map.empty infos
      _ <- Map.traverseWithKey detectDuplicateNames nameTable
      return infos


readManyFilesHelp :: Summary -> FilePath -> Task.Task_ E.Error (Module.Raw, Info)
readManyFilesHelp summary path =
  do  time <- liftIO $ Dir.getModificationTime path
      source <- liftIO $ IO.readUtf8 path
      (maybeName, info) <- atRoot $ parse (_project summary) path time source
      case maybeName of
        Nothing ->
          Task.throw (E.RootNameless path)

        Just name ->
          return (name, info)


detectDuplicateNames :: Module.Raw -> NonEmpty Info -> Task.Task_ E.Error ()
detectDuplicateNames name (info :| otherInfos) =
  case otherInfos of
    [] ->
      return ()

    _ ->
      Task.throw (E.RootModuleNameDuplicate name (map _path (info : otherInfos)))




-- READ SOURCE


readSource :: Project -> Text.Text -> Task.Task (Maybe Module.Raw, Info)
readSource project source =
  Task.mapError Error.Crawl $
    atRoot $ parse project "elm" fakeTime source


fakeTime :: Time.UTCTime
fakeTime =
  Time.UTCTime (Day.fromGregorian 2990 2 2) 0



-- PARSE HEADER


parse :: Project -> FilePath -> Time.UTCTime -> Text.Text -> Task.Task_ E.Problem (Maybe Module.Raw, Info)
parse project path time source =
  -- TODO get regions on data extracted here
  case Compiler.parseHeader (Project.getName project) source of
    Right (maybeDecl, deps) ->
      do  maybeName <- checkTag project maybeDecl
          return ( maybeName, Info path time source deps )

    Left msg ->
      Task.throw (E.BadHeader path source msg)


checkTag :: Project -> Maybe (Compiler.Tag, Module.Raw) -> Task.Task_ E.Problem (Maybe Module.Raw)
checkTag project maybeDecl =
  case maybeDecl of
    Nothing ->
      return Nothing

    Just (tag, name) ->
      do  check
          return (Just name)
      where
        check =
          case tag of
            Compiler.Normal ->
              return ()

            Compiler.Port ->
              case project of
                Project.App _ ->
                  return ()

                Project.Pkg _ ->
                  Task.throw (E.PortsInPackage name)

            Compiler.Effect ->
              if Project.getEffect project then
                return ()

              else
                Task.throw (E.EffectsUnexpected name)
