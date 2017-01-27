{-# OPTIONS_GHC -Wall #-}
module Deps.Explorer
  ( Metadata, Cons(..)
  , Explorer, run
  , getVersions
  , getContraints
  , getPackageInfo
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
import Control.Monad.RWS (lift, liftIO, gets, modify)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Directory as Dir

import Elm.Package (Name, Version)

import qualified Deps.Website as Website
import qualified Elm.Project as Project
import Elm.Project.Constraint (Constraint)
import qualified File.IO as IO
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task



-- METADATA CACHE


data Metadata =
  Metadata
    { _vsns :: Map Name [Version]
    , _cons :: Map (Name, Version) Cons
    }


data Cons =
  Cons
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
          throwError (Error.Assets (E.CorruptVersionCache name))



-- CONSTRAINTS


getContraints :: Name -> Version -> Explorer Cons
getContraints name version =
  do  allCons <- gets _cons
      case Map.lookup (name, version) allCons of
        Just cons ->
          return cons

        Nothing ->
          do  info <- lift $ getPackageInfo name version

              -- extract constraints
              let elm = Project._pkg_elm_version info
              let pkgs = Project._pkg_dependencies info
              let cons = Cons elm pkgs

              -- Keep the cons in memory in case we need them again
              modify $ \store ->
                store { _cons = Map.insert (name, version) cons (_cons store) }

              return cons



-- PACKAGE INFO


getPackageInfo :: Name -> Version -> Task.Task Project.PkgInfo
getPackageInfo name version =
  do  infoPath <- Task.getPackageInfoPath name version
      exists <- liftIO $ Dir.doesFileExist infoPath
      if exists
        then IO.readBinary infoPath
        else
          do  info <- Website.getPkgInfo name version
              IO.writeBinary infoPath info
              return info
