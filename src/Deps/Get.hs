{-# OPTIONS_GHC -Wall #-}
module Deps.Get
  ( all
  , info
  )
  where

import Prelude hiding (all)
import Control.Monad.Except (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified System.Directory as Dir
import System.FilePath ((</>))

import Elm.Package (Name, Version)

import qualified Deps.Website as Website
import qualified Elm.Project as Project
import qualified File.IO as IO
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task



-- ALL VERSIONS


all :: Task.Task (Map Name [Version])
all =
  do  dir <- Task.getPackageCacheDir
      let versionsFile = dir </> "versions.dat"
      exists <- liftIO $ Dir.doesFileExist versionsFile
      if exists
        then fetchNew versionsFile
        else fetchAll versionsFile


fetchAll :: FilePath -> Task.Task (Map Name [Version])
fetchAll versionsFile =
  do  (size, packages) <- Website.getAllPackages
      IO.writeBinary versionsFile (size, packages)
      return packages


fetchNew :: FilePath -> Task.Task (Map Name [Version])
fetchNew versionsFile =
  do  (size, packages) <- IO.readBinary versionsFile

      news <- Website.getNewPackages size

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

      case Project.parse json of
        Right (Project.Pkg pkgInfo) ->
          return pkgInfo

        Right (Project.App _) ->
          do  IO.remove elmJson
              Task.throw $ Error.Assets $ E.CorruptProject elmJson Nothing

        Left err ->
          do  IO.remove elmJson
              Task.throw $ Error.Assets $ E.CorruptProject elmJson err
