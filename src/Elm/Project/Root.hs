{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Root
  ( get
  , getWithReplFallback
  )
  where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified Elm.Project.BuildPlan as BP
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Licenses as Licenses
import Elm.Project.Json (Project(..), PkgInfo(..))
import Elm.Project.Summary (Summary)
import qualified Elm.PerUserCache as PerUserCache
import qualified File.IO as IO
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task
import qualified Stuff.Verify as Verify
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- GET


get :: Task.Task Summary
get =
  do  root <- moveToRoot
      project <- readProject
      Verify.verify root project



-- MOVE TO ROOT


moveToRoot :: Task.Task FilePath
moveToRoot =
  do  maybeRoot <- liftIO findRoot
      case maybeRoot of
        Just root ->
          do  liftIO $ Dir.setCurrentDirectory root
              return root

        Nothing ->
          Task.throw Error.NoElmJson


findRoot :: IO (Maybe FilePath)
findRoot =
  do  subDir <- Dir.getCurrentDirectory
      findRootHelp (FP.splitDirectories subDir)


findRootHelp :: [String] -> IO (Maybe FilePath)
findRootHelp dirs =
  if null dirs then
    return Nothing

  else
    do  exists <- Dir.doesFileExist (FP.joinPath dirs </> "elm.json")
        if exists
          then return (Just (FP.joinPath dirs))
          else findRootHelp (init dirs)



-- READ PROJECT


readProject :: Task.Task Project
readProject =
  do  elmBits <- liftIO $ BS.readFile "elm.json"
      case Decode.parse Project.decoder elmBits of
        Left Nothing ->
          throw E.BadSyntax

        Left (Just err) ->
          throw (E.BadStructure err)

        Right (Project.RawPkg info) ->
          case Project.checkPkgOverlap info of
            Just overlap ->
              throw E.BadContent

            Nothing ->
              return (Project.Pkg info)

        Right (Project.RawApp info) ->
          case Project.checkAppOverlap info of
            Just overlap ->
              throw E.BadContent

            Nothing ->
              Project.App info <$> maybeReadBuildPlan


throw :: E.JsonProblem -> Task.Task a
throw problem =
  Task.throw (Error.Assets (E.BadElmJson problem))



-- READ BUILD PLAN


maybeReadBuildPlan :: Task.Task (Maybe BP.BuildPlan)
maybeReadBuildPlan =
  do  exists <- IO.exists "elm-build-plan.json"
      if exists
        then Just <$> readBuildPlan
        else return Nothing


readBuildPlan :: Task.Task BP.BuildPlan
readBuildPlan =
  BP.parse =<< liftIO (BS.readFile "elm-build-plan.json")



-- GET WITH FALLBACK


getWithReplFallback :: IO (Maybe FilePath)
getWithReplFallback =
  do  maybeRoot <- findRoot

      case maybeRoot of
        Just root ->
          Dir.setCurrentDirectory root

        Nothing ->
          do  cache <- PerUserCache.getReplRoot
              let root = cache </> "tmp"
              Dir.setCurrentDirectory root
              IO.removeDir "elm-stuff"
              Encode.write "elm.json" (Project.encode (Pkg replInfo))

      return maybeRoot


replInfo :: PkgInfo
replInfo =
  PkgInfo
    { _pkg_name = Pkg.dummyName
    , _pkg_summary = "dummy code for the REPL"
    , _pkg_license = Licenses.bsd3
    , _pkg_version = Pkg.dummyVersion
    , _pkg_exposed = []
    , _pkg_deps = Map.singleton Pkg.core Con.anything
    , _pkg_test_deps = Map.empty
    , _pkg_elm_version = Con.defaultElm
    , _pkg_effects = False
    }
