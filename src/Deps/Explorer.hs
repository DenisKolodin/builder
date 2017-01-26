{-# OPTIONS_GHC -Wall #-}
module Deps.Explorer
  ( Metadata, Deps(..)
  , Explorer, run
  , getVersions, getDeps
  )
  where

{-| It is expensive to load ALL package metadata. You would need to:

  1. Know all the packages and all their versions.
  2. Download or read the elm.json file for each version.

The goal of this module is to only pay for (1) and pay for (2) as needed.
-}


import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.RWS (lift, liftIO, asks, gets, modify)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Dependencies.Website as Website
import Elm.Package (Name, Version)
import Elm.Package.Constraint (Constraint)
import qualified Elm.Config as Config
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- METADATA CACHE


data Metadata =
  Metadata
    { _vsns :: Map Name [Version]
    , _deps :: Map (Name, Version) Deps
    }


data Deps =
  Deps
    { _elm :: Constraint
    , _pkgs :: Map Name Constraint
    }



-- EXPLORER


type Explorer a =
  StateT Metadata Task.Task a


run :: Explorer a -> Task.Task a
run explorer =
  do  versions <- fetchVersions
      evalStateT explorer (Metadata versions Map.empty)


fetchVersions :: Task.Task (Map Name [Version])
fetchVersions =
  do  versionsFile <- Task.getVersionsPath
      exists <- liftIO $ Dir.doesFileExist versionsFile
      if exists
        then fetchNewVersions versionsFile
        else fetchAllVersions versionsFile


fetchAllVersions :: FilePath -> Task.Task (Map Name [Version])
fetchAllVersions versionsFile =
  do  (time, packages) <- Website.getAllPackages

      -- create versions.dat with all the package info
      liftIO $
        BS.writeFile versionsFile $
          Binary.encode (time, packages)

      return packages


-- assumes that `versionsFile` exists
fetchNewVersions :: FilePath -> Task.Task (Map Name [Version])
fetchNewVersions versionsFile =
  do  -- read everything that is cached locally
      (oldTime, oldPackages) <- liftIO (Binary.decode <$> BS.readFile versionsFile)

      -- attempt to get the latest versions from <pagkage.elm-lang.org>
      (newTime, newPackages) <- Website.getNewPackages oldTime

      -- combine the old and new packages
      let allPackages = addNewPkgs oldPackages newPackages

      -- update versions.dat with new info
      when (not (null newPackages)) $
        liftIO $ BS.writeFile versionsFile $
          Binary.encode (newTime, allPackages)

      return allPackages


addNewPkgs :: Map Name [Version] -> [(Name, Version)] -> Map Name [Version]
addNewPkgs oldPackages newPackages =
  let
    add (name, vsn) pkgs =
      Map.insertWith (\old new -> new ++ old) name [vsn] pkgs
  in
    foldr add oldPackages newPackages



-- VERSIONS


getVersions :: Name -> Explorer [Version]
getVersions name =
  do  allVersions <- gets _vsns
      case Map.lookup name allVersions of
        Just versions ->
          return versions

        Nothing ->
          throwError $ Error.CorruptVersionCache name



-- GET DEPENDENCIES


getDeps :: Name -> Version -> Explorer Deps
getDeps name version =
  do  allDeps <- gets _deps
      case Map.lookup (name, version) allDeps of
        Just deps ->
          return deps

        Nothing ->
          do  configPath <- lift $ Task.getConfigPath name version
              exists <- liftIO $ Dir.doesFileExist configPath

              -- get the config information
              config <-
                lift $
                  if exists
                    then Config.forcePkg =<< Config.unsafeRead configPath
                    else fetchConfig name version configPath

              -- just extract the dependencies
              let deps =
                    Deps
                      (Config._pkg_elm_version config)
                      (Config._pkg_dependencies config)

              -- Keep the deps in memory in case we need them again
              modify $ \store ->
                store { _deps = Map.insert (name, version) deps (_deps store) }

              return deps


fetchConfig :: Name -> Version -> FilePath -> Task.Task Config.PkgConfig
fetchConfig name version configPath =
  do  config <- Website.getPkgConfig name version
      liftIO $ Config.write configPath (Config.Pkg config)
      return config

