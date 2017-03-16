{-# OPTIONS_GHC -Wall #-}
module Deps.Explorer
  ( Explorer
  , Metadata
  , Info(..)
  , run
  , getVersions
  , getConstraints
  )
  where

{-| It is expensive to load ALL package metadata. You would need to:

  1. Know all the packages and all their versions.
  2. Download or read the elm.json file for each version.

The goal of this module is to only pay for (1) and pay for (2) as needed.
-}


import Control.Monad.Except (lift, throwError)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map

import Elm.Package (Name, Version)

import qualified Deps.Get as Get
import qualified Elm.Project.Json as Project
import Elm.Project.Constraint (Constraint)
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task



-- EXPLORER


type Explorer =
  StateT Metadata Task.Task


data Metadata =
  Metadata
    { _vsns :: Map Name [Version]
    , _info :: Map (Name, Version) Info
    }


data Info =
  Info
    { _elm :: Constraint
    , _pkgs :: Map Name Constraint
    }


run :: Explorer a -> Task.Task a
run explorer =
  do  versions <- Get.all
      evalStateT explorer (Metadata versions Map.empty)



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


getConstraints :: Name -> Version -> Explorer Info
getConstraints name version =
  do  allInfo <- gets _info
      case Map.lookup (name, version) allInfo of
        Just info ->
          return info

        Nothing ->
          do  pkgInfo <- lift $ Get.info name version

              let elm = Project._pkg_elm_version pkgInfo
              let pkgs = Project._pkg_deps pkgInfo
              let info = Info elm pkgs

              modify $ \(Metadata vsns infos) ->
                Metadata vsns $ Map.insert (name, version) info infos

              return info
