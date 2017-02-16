{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
module Stuff.Info
  ( DepsInfo
  , getPackages
  , getDepModules, DepModules
  , verify
  )
  where


import Data.Binary (Binary)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import qualified Deps.Get as Get
import qualified Deps.Verify as Verify
import Elm.Project (Project(..), AppInfo(..), PkgInfo(..))
import qualified Elm.Project as Project
import qualified Elm.Project.Constraint as Con
import qualified File.IO as IO
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- DEPS INFO


data DepsInfo =
  DepsInfo
    { _deps :: [Project.PkgInfo]
    }
    deriving (Generic)


instance Binary DepsInfo


getPackages :: DepsInfo -> [(Name,Version)]
getPackages (DepsInfo deps) =
  flip map deps $ \info ->
    ( _pkg_name info, _pkg_version info )



-- DEPENDENCY MODULES


type DepModules =
  Map.Map Module.Raw [(Name, Version)]


getDepModules :: Project -> DepsInfo -> DepModules
getDepModules project (DepsInfo depsInfo) =
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


insertPkg :: DepModules -> Project.PkgInfo -> DepModules
insertPkg depModules info =
  let
    home =
      ( _pkg_name info, _pkg_version info )

    insertModule dict modul =
      Map.insertWith (++) modul [home] dict
  in
    List.foldl' insertModule depModules (_pkg_exposed info)



-- VERIFY


verify :: Project -> Task.Task DepsInfo
verify project =
  let
    safeIsValid =
      Task.try False $
        do  cacheProject <- IO.readBinary Path.pkgInfo
            return (isValid cacheProject project)
  in
    do  valid <- safeIsValid
        if valid
          then IO.readBinary Path.deps
          else rebuildCache project



-- DOES PROJECT MATCH CACHE?


isValid :: Project -> Project -> Bool
isValid p1 p2 =
  case (p1, p2) of
    (App info1, App info2) ->
      _app_deps info1 == _app_deps info2
      && _app_elm_version info1 == _app_elm_version info2
      && _app_elm_version info1 == Compiler.version

    (Pkg info1, Pkg info2) ->
      _pkg_transitive_deps info1 == _pkg_transitive_deps info2
      && _pkg_dependencies info1 == _pkg_dependencies info2
      && _pkg_test_deps info1 == _pkg_test_deps info2
      && _pkg_elm_version info1 == _pkg_elm_version info2
      && Con.goodElm (_pkg_elm_version info1)

    _ ->
      False



-- ACTUALLY VALIDATE


rebuildCache :: Project -> Task.Task DepsInfo
rebuildCache project =
  do  -- get rid of cached information
      -- TODO build artifacts too?
      IO.remove Path.pkgInfo
      IO.remove Path.deps
      IO.remove Path.ifaces

      Verify.verify project

      let solution = Project.toSolution (Project.getTransDeps project)
      depsInfo <- Map.elems <$> Map.traverseWithKey Get.info solution

      IO.writeBinary Path.pkgInfo project
      IO.writeBinary Path.deps depsInfo
      return (DepsInfo depsInfo)

