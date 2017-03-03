{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
module Stuff.Deps
  ( Info(..)
  , getNames
  , Modules
  , getModules
  )
  where


import Data.Binary (Binary)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)

import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import Elm.Project (Project(..), PkgInfo(..))
import qualified Elm.Project as Project



-- DEPS INFO


data Info =
  Info
    { _deps :: [Project.PkgInfo]
    }
    deriving (Generic)


instance Binary Info


getNames :: Info -> [(Name,Version)]
getNames (Info deps) =
  flip map deps $ \info ->
    ( _pkg_name info, _pkg_version info )



-- DEPENDENCY MODULES


type Modules =
  Map.Map Module.Raw [(Name, Version)]


getModules :: Project -> Info -> Modules
getModules project (Info depsInfo) =
  let
    (Project.TransitiveDeps deps _ _ _) =
      Project.getTransDeps project

    directNames =
      Map.keysSet deps

    isDirect info =
      Set.member (_pkg_name info) directNames

    directDeps =
      filter isDirect depsInfo
  in
    List.foldl' insertPkg Map.empty directDeps


insertPkg :: Modules -> Project.PkgInfo -> Modules
insertPkg depModules info =
  let
    home =
      ( _pkg_name info, _pkg_version info )

    insertModule dict modul =
      Map.insertWith (++) modul [home] dict
  in
    List.foldl' insertModule depModules (_pkg_exposed info)

