module Deps.Validate
  ( validate
  )
  where


import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler
import Elm.Package (Name, Version)

import qualified Deps.Explorer as Explorer
import Deps.Info as Deps
import Elm.Project (Project, PkgInfo, ExactDeps)
import qualified Elm.Project as Project
import qualified Elm.Project.Constraint as C
import qualified File.IO as IO
import qualified Reporting.Task as Task



-- VALIDATE


validate :: Project -> Task.Task Deps.Info
validate project =
  let
    -- TODO - get rid of cacheDir configuration
    cacheDir =
      Project.toCacheDir project

    safeIsValid =
      Task.try False $
        do  cacheProject <- IO.readBinary (cacheDir </> "elm.dat")
            return (isValid cacheProject project)
  in
    do  valid <- safeIsValid
        if valid
          then IO.readBinary (cacheDir </> "deps.dat")
          else rebuildCache cacheDir project



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


rebuildCache :: FilePath -> Project -> Task.Task Deps.Info
rebuildCache cacheDir project =
  -- gather transitive dependencies
  -- read all of their elm.json files
  -- make sure all constraints are satisfied
  --   if yes, write new elm.dat and deps.dat / return info
  --   if no, error or "would you like me to revert to the last valid state?"

  do  -- get rid of cached information
      -- TODO build artifacts too?
      IO.remove (cacheDir </> "elm.dat")
      IO.remove (cacheDir </> "deps.dat")
      IO.remove (cacheDir </> "ifaces.dat")

      -- validate solution
      let solution = Project.toExactDeps project
      depsInfo <- Deps.fromInfoList <$>
        traverse (readDep solution) (Map.toList solution)

      IO.writeBinary (cacheDir </> "elm.dat") project
      IO.writeBinary (cacheDir </> "deps.dat") depsInfo
      writeInterfaces (cacheDir </> "ifaces.dat") solution

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



-- WRITE INTERFACES


writeInterfaces :: FilePath -> ExactDeps -> Task.Task ()
writeInterfaces ifacePath solution =
  error "TODO" ifacePath solution

