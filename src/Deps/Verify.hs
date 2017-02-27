module Deps.Verify (verify) where


import Control.Concurrent.MVar (readMVar)
import Control.Monad (filterM, forM)
import Control.Monad.Trans (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import Elm.Package (Name, Version)
import Elm.Project (Project(..), AppInfo(..), PkgInfo(..))
import Elm.Project.Constraint (Constraint)

import qualified Deps.Explorer as Explorer
import qualified Deps.Solver.Internal as Solver
import qualified Deps.Website as Website
import qualified Elm.Compiler as Compiler
import qualified Elm.Project as Project
import qualified Elm.Project.Constraint as Con
import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task



-- VERIFY


verify :: Project -> Task.Task ()
verify project =
  do  solution <- Project.get verifyApp verifyPkg project
      verifySolution solution



-- VERIFY APP


verifyApp :: AppInfo -> Task.Task (Map Name Version)
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
            return solution



-- VERIFY PKG


verifyPkg :: PkgInfo -> Task.Task (Map Name Version)
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
                return solution


verifyPkgHelp :: Map Name Version -> Name -> Constraint -> Task.Task ()
verifyPkgHelp solution name constraint =
  case Map.lookup name solution of
    Just version | Con.satisfies constraint version ->
      return ()

    _ ->
      Task.throw Error.PkgBadDeps



-- VERIFY SOLUTION


verifySolution :: Map Name Version -> Task.Task ()
verifySolution solution =
  do  let pkgs = Map.toList solution
      noSrc <- filterM (no "src") pkgs

      download noSrc

      noStuff <- filterM (no "elm-stuff") pkgs

      -- see if it all builds against itself

      error "TODO"


no :: FilePath -> (Name, Version) -> Task.Task Bool
no dir (name, version) =
  do  root <- Task.getPackageCacheDirFor name version
      liftIO $ not <$> doesDirectoryExist (root </> dir)


download :: [(Name, Version)] -> Task.Task ()
download packages =
  case packages of
    [] ->
      return ()

    _ : _ ->
      do  Task.report (Progress.DownloadStart packages)
          mvars <- forM packages $ \(name, version) ->
            Task.workerMVar $ Website.download name version

          results <- liftIO $ traverse readMVar mvars

          case sequence results of
            Left err ->
              do  Task.report (Progress.DownloadEnd Progress.Bad)
                  Task.throw err

            Right abc ->
              Task.report (Progress.DownloadEnd Progress.Good)
