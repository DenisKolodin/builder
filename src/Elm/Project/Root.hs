{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Root
  ( get
  , unsafeGet
  , getWithReplFallback
  )
  where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Licenses as Licenses
import Elm.Project.Json (Project(..), PkgInfo(..))
import Elm.Project.Summary (Summary)
import qualified Elm.PerUserCache as PerUserCache
import qualified File.IO as IO
import qualified Generate.Plan as Plan
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


unsafeGet :: Task.Task (FilePath, Project)
unsafeGet =
  (,) <$> moveToRoot <*> readProject



-- MOVE TO ROOT


moveToRoot :: Task.Task FilePath
moveToRoot =
  do  maybeRoot <- liftIO $ IO.find "elm.json"
      case maybeRoot of
        Just root ->
          do  liftIO $ Dir.setCurrentDirectory root
              return root

        Nothing ->
          Task.throw Error.NoElmJson



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
              Project.App info <$> maybeReadPlan


throw :: E.JsonProblem -> Task.Task a
throw problem =
  Task.throw (Error.Assets (E.BadElmJson problem))



-- READ BUILD PLAN


maybeReadPlan :: Task.Task (Maybe Plan.Plan)
maybeReadPlan =
  do  exists <- IO.exists "elm-build-plan.json"
      if exists
        then Just <$> readPlan
        else return Nothing


readPlan :: Task.Task Plan.Plan
readPlan =
  Plan.parse =<< liftIO (BS.readFile "elm-build-plan.json")



-- GET WITH FALLBACK


getWithReplFallback :: IO FilePath
getWithReplFallback =
  do  maybeRoot <- IO.find "elm.json"

      case maybeRoot of
        Just root ->
          do  Dir.setCurrentDirectory root
              return root

        Nothing ->
          do  cache <- PerUserCache.getReplRoot
              let root = cache </> "tmp"
              Dir.createDirectoryIfMissing True root
              Dir.setCurrentDirectory root
              IO.removeDir "elm-stuff"
              Encode.write "elm.json" (Project.encode (Pkg replInfo))
              return root


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
