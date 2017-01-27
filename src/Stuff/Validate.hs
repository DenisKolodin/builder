{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
module Stuff.Validate
  ( DepsInfo
  , ExposedModules
  , getExposedModules
  , validate
  )
  where


import Data.Binary (Binary)
import qualified Data.Map as Map
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



-- EXPOSED MODULES


type ExposedModules =
  Map.Map Module.Raw [(Name, Version)]


getExposedModules :: DepsInfo -> ExposedModules
getExposedModules (DepsInfo deps) =
  foldr insertPkg Map.empty deps


insertPkg :: Project.PkgInfo -> ExposedModules -> ExposedModules
insertPkg info exposedModules =
  let
    home =
      ( Project.toPkgName info
      , Project._pkg_version info
      )

    insertModule modul dict =
      Map.insertWith (++) modul [home] dict
  in
    foldr insertModule exposedModules (Project._pkg_exposed info)



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


-- TODO - detect elm.json changes more cleverly
--
-- Full equality is not actually necessary. It is fine if the
-- summary or license changes. Only changes in dependencies
-- really matter.
isValid :: Project -> Project -> Bool
isValid p1 p2 =
  p1 == p2 && Project.matchesCompilerVersion p1



-- ACTUALLY VALIDATE


rebuildCache :: Project -> Task.Task DepsInfo
rebuildCache project =
  -- gather transitive dependencies
  -- read all of their elm.json files
  -- make sure all constraints are satisfied
  --   if yes, write new elm.dat and deps.dat / return info
  --   if no, error or "would you like me to revert to the last valid state?"

  do  -- get rid of cached information
      -- TODO build artifacts too?
      IO.remove Path.pkgInfo
      IO.remove Path.deps
      IO.remove Path.ifaces

      -- validate solution
      let solution = Project.toExactDeps project
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
    Task.throw (error "TODO")


checkDep :: ExactDeps -> Name -> Name -> C.Constraint -> Task.Task ()
checkDep solution depName subDepName constraint =
  case Map.lookup subDepName solution of
    Nothing ->
      Task.throw (error "TODO" depName subDepName)

    Just version ->
      if C.isSatisfied constraint version then
        return ()

      else
        Task.throw (error "TODO" depName subDepName)
