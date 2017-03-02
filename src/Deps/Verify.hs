module Deps.Verify (verify) where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (filterM, forM, void)
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


verify :: Project -> Task.Task (Map Name PkgInfo)
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


verifySolution :: Map Name Version -> Task.Task (Map Name PkgInfo)
verifySolution solution =
  do  download =<< filterM (no "src") (Map.toList solution)
      verifyBuildArtifacts solution


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

          case sequence_ results of
            Left err ->
              do  Task.report (Progress.DownloadEnd Progress.Bad)
                  Task.throw err

            Right _ ->
              Task.report (Progress.DownloadEnd Progress.Good)



-- VERIFY BUILD ARTIFACTS


verifyBuildArtifacts :: Map Name Version -> Task.Task (Map Name PkgInfo)
verifyBuildArtifacts solution =
  do  mvar <- liftIO newEmptyMVar
      pkgInfo <- Map.traverseWithKey (verifyBuild mvar) solution
      liftIO $ putMVar mvar pkgInfo
      liftIO $ traverse readMVar pkgInfo


verifyBuild :: MVar (Map Name (MVar PkgInfo)) -> Name -> Version -> Task.Task (MVar PkgInfo)
verifyBuild pkgInfoMVar name version =
  do  mvar <- liftIO newEmptyMVar

      liftIO $ void $ forkIO $
        do  readMVar pkgInfoMVar
            error "TODO actually compile this project"

      return mvar