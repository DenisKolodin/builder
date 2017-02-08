{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
module Stuff.Validate
  ( DepsInfo
  , getPackages
  , getDepModules, DepModules
  , validate
  )
  where


import Data.Binary (Binary)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import qualified Deps.Explorer as Explorer
import Elm.Project (Project, PkgInfo, ExactDeps)
import qualified Elm.Project as Project
import qualified Elm.Project.Constraint as C
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
    ( Project.toPkgName info
    , Project.toPkgVersion info
    )



-- DEPENDENCY MODULES


type DepModules =
  Map.Map Module.Raw [(Name, Version)]


getDepModules :: Project -> DepsInfo -> DepModules
getDepModules project (DepsInfo deps) =
  let
    directSet =
      Project.toDirectDeps project

    isDirect info =
      Set.member (Project.toPkgName info) directSet

    directDeps =
      filter isDirect deps
  in
    foldr insertPkg Map.empty directDeps


insertPkg :: Project.PkgInfo -> DepModules -> DepModules
insertPkg info depModules =
  let
    home =
      ( Project.toPkgName info
      , Project._pkg_version info
      )

    insertModule modul dict =
      Map.insertWith (++) modul [home] dict
  in
    foldr insertModule depModules (Project._pkg_exposed info)



-- VALIDATE


validate :: Project -> Task.Task DepsInfo
validate project =
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


{-| The project is considered valid if:

  - All *transitive* dependencies are the same. That means
    all the same packages with the exact same versions.

  - All *direct* dependencies are the same. That means
    of the transitive dependencies, the same subset of
    packages is available for use by your project.

This means you can change version, summaries, licenses,
output directories, etc. We just need to be sure that
elm.dat, deps.dat, and ifaces.dat are all correct.
-}
isValid :: Project -> Project -> Bool
isValid p1 p2 =
  Project.matchesCompilerVersion p1
  && Project.toSolution p1 == Project.toSolution p2
  && Project.toDirectDeps p1 == Project.toDirectDeps p2



-- ACTUALLY VALIDATE


rebuildCache :: Project -> Task.Task DepsInfo
rebuildCache project =
  do  -- get rid of cached information
      -- TODO build artifacts too?
      IO.remove Path.pkgInfo
      IO.remove Path.deps
      IO.remove Path.ifaces

      -- validate solution
      let solution = Project.toSolution project
      depsInfo <- DepsInfo <$>
        traverse (readDep solution) (Map.toList solution)

      IO.writeBinary Path.pkgInfo project
      IO.writeBinary Path.deps depsInfo

      return depsInfo



-- VALIDATE INDIVIDUAL DEPENDENCY


readDep :: ExactDeps -> (Name, Version) -> Task.Task PkgInfo
readDep solution (name, version) =
  do  info <- Explorer.getPackageInfo name version
      _ <- checkCompilerVersion info
      _ <- Map.traverseWithKey (checkDep solution name) (Project._pkg_dependencies info)
      _ <- Map.traverseWithKey (checkDep solution name) (Project._pkg_test_deps info)
      return info


checkCompilerVersion :: PkgInfo -> Task.Task ()
checkCompilerVersion info =
  if C.isSatisfied (Project._pkg_elm_version info) Compiler.version then
    return ()
  else
    Task.throw (error "TODO checkCompilerVersion")


checkDep :: ExactDeps -> Name -> Name -> C.Constraint -> Task.Task ()
checkDep solution depName subDepName constraint =
  case Map.lookup subDepName solution of
    Nothing ->
      Task.throw (error "TODO checkDep" depName subDepName)

    Just version ->
      if C.isSatisfied constraint version then
        return ()

      else
        Task.throw (error "TODO checkDep" depName subDepName)
