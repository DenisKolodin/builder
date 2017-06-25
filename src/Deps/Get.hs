{-# OPTIONS_GHC -Wall #-}
module Deps.Get
  ( all
  , Mode(..)
  , info
  , docs
  )
  where

import Prelude hiding (all)
import Control.Monad.Except (catchError, liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Docs as Docs
import Elm.Package (Name, Version)

import qualified Deps.Website as Website
import qualified Elm.Project.Json as Project
import qualified File.IO as IO
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Json.Decode as Decode



-- ALL VERSIONS


data Mode = RequireLatest | AllowOffline


all :: Mode -> Task.Task (Map Name [Version])
all mode =
  do  dir <- Task.getPackageCacheDir
      let versionsFile = dir </> "versions.dat"
      exists <- liftIO $ Dir.doesFileExist versionsFile
      if exists
        then fetchNew versionsFile mode
        else fetchAll versionsFile


fetchAll :: FilePath -> Task.Task (Map Name [Version])
fetchAll versionsFile =
  do  packages <- Website.getAllPackages
      let size = Map.foldr ((+) . length) 0 packages
      IO.writeBinary versionsFile (size, packages)
      return packages


fetchNew :: FilePath -> Mode -> Task.Task (Map Name [Version])
fetchNew versionsFile mode =
  do  (size, packages) <- IO.readBinary versionsFile

      news <-
        case mode of
          RequireLatest ->
            Website.getNewPackages size

          AllowOffline ->
            Website.getNewPackages size `catchError` \_ ->
              do  Task.report Progress.UnableToLoadLatestPackages
                  return []

      if null news
        then return packages
        else
          do  let packages' = List.foldl' addNew packages news
              IO.writeBinary versionsFile (size + length news, packages')
              return packages'


addNew :: Map Name [Version] -> (Name, Version) -> Map Name [Version]
addNew packages (name, version) =
  Map.insertWith (++) name [version] packages



-- PACKAGE INFO


info :: Name -> Version -> Task.Task Project.PkgInfo
info name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let elmJson = dir </> "elm.json"

      exists <- liftIO $ Dir.doesFileExist elmJson

      json <-
        if exists
          then liftIO (BS.readFile elmJson)
          else
            do  bits <- Website.getElmJson name version
                liftIO (BS.writeFile elmJson bits)
                return bits

      case Decode.parse Project.pkgDecoder json of
        Right pkgInfo ->
          return pkgInfo

        Left _ ->
          do  IO.remove elmJson
              Task.throw $ Error.Assets $ E.CorruptElmJson name version



-- DOCS


docs :: Name -> Version -> Task.Task Docs.Documentation
docs name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let docsJson = dir </> "docs.json"

      exists <- liftIO $ Dir.doesFileExist docsJson

      json <-
        if exists
          then liftIO (BS.readFile docsJson)
          else
            do  bits <- Website.getDocs name version
                liftIO (BS.writeFile docsJson bits)
                return bits

      case Decode.parse (Docs.toDict <$> Decode.list Docs.decoder) json of
        Right pkgInfo ->
          return pkgInfo

        Left _ ->
          do  IO.remove docsJson
              Task.throw $ Error.Assets $ E.CorruptDocumentation name version
