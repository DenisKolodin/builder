module Deps.Verify (verify) where

import Data.Map (Map)
import qualified Data.Map as Map

import Elm.Package (Name, Version)
import Elm.Project (Project(..), AppInfo(..), PkgInfo(..))
import Elm.Project.Constraint (Constraint)

import qualified Deps.Explorer as Explorer
import qualified Deps.Solver.Internal as Solver
import qualified Elm.Compiler as Compiler
import qualified Elm.Project as Project
import qualified Elm.Project.Constraint as Con
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- VERIFY


verify :: Project -> Task.Task ()
verify project =
  case project of
    App info ->
      verifyApp info

    Pkg info ->
      verifyPkg info



-- VERIFY APP


verifyApp :: AppInfo -> Task.Task ()
verifyApp info =
  if _app_elm_version info /= Compiler.version then
    Task.throw (Error.AppBadElm (_app_elm_version info))

  else
    do  let oldSolution = Project.toSolution (_app_deps info)
        let solver = Solver.solve (Map.map Con.exactly oldSolution)
        maybeSolution <- Explorer.run (Solver.run solver)
        case maybeSolution of
          Nothing ->
            Task.throw Error.AppBadDeps

          Just solution ->
            return ()



-- VERIFY PKG


verifyPkg :: PkgInfo -> Task.Task ()
verifyPkg info =
  if Con.goodElm (_pkg_elm_version info) then
    Task.throw (Error.PkgBadElm (_pkg_elm_version info))

  else
    do  let oldSolution = Project.toSolution (_pkg_transitive_deps info)
        let solver = Solver.solve (Map.map Con.exactly oldSolution)
        maybeSolution <- Explorer.run (Solver.run solver)
        case maybeSolution of
          Nothing ->
            Task.throw Error.PkgBadDeps

          Just solution ->
            do  let check = Map.traverseWithKey (verifyPkgHelp solution)
                _ <- check (_pkg_dependencies info)
                _ <- check (_pkg_test_deps info)
                return ()


verifyPkgHelp :: Map Name Version -> Name -> Constraint -> Task.Task ()
verifyPkgHelp solution name constraint =
  case Map.lookup name solution of
    Just version | Con.satisfies constraint version ->
      return ()

    _ ->
      Task.throw Error.PkgBadDeps
