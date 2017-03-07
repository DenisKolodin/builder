module Deps.Verify (verify) where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (filterM, forM, void)
import Control.Monad.Trans (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)
import Elm.Project (Project(..), AppInfo(..), PkgInfo(..))
import Elm.Project.Constraint (Constraint)

import qualified Deps.Explorer as Explorer
import qualified Deps.Get as Get
import qualified Deps.Interface as Interface
import qualified Deps.Solver.Internal as Solver
import qualified Deps.Website as Website
import qualified Elm.Compiler as Compiler
import qualified Elm.Project as Project
import qualified Elm.Project.Constraint as Con
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff.Deps as Deps



-- VERIFY


verify :: Project -> Task.Task (Map Name Version, Deps.Summary)
verify project =
  do  solution <- Project.get verifyApp verifyPkg project
      (RawInfo infos ifaces) <- verifyArtifacts solution
      let summary = Deps.makeSummary project infos ifaces
      return (solution, summary)



-- VERIFY SOLUTION


verifyApp :: AppInfo -> Task.Task (Map Name Version)
verifyApp info =
  if _app_elm_version info /= Compiler.version then
    Task.throw (Error.AppBadElm (_app_elm_version info))

  else
    do  let oldSolution = Project.appSolution info
        let solver = Solver.solve (Map.map Con.exactly oldSolution)
        maybeSolution <- Explorer.run (Solver.run solver)
        case maybeSolution of
          Nothing ->
            Task.throw Error.AppBadDeps

          Just solution ->
            return solution


verifyPkg :: PkgInfo -> Task.Task (Map Name Version)
verifyPkg info =
  if not (Con.goodElm (_pkg_elm_version info)) then
    Task.throw (Error.PkgBadElm (_pkg_elm_version info))

  else
    do  let deps = Map.union (_pkg_deps info) (_pkg_test_deps info)
        let solver = Solver.solve deps
        maybeSolution <- Explorer.run (Solver.run solver)
        case maybeSolution of
          Nothing ->
            Task.throw Error.PkgBadDeps

          Just solution ->
            return solution



-- VERIFY ARTIFACTS


verifyArtifacts :: Map Name Version -> Task.Task RawInfo
verifyArtifacts solution =
  do  download =<< filterM noSrc (Map.toList solution)
      verifyBuildArtifacts solution


noSrc :: (Name, Version) -> Task.Task Bool
noSrc (name, version) =
  do  root <- Task.getPackageCacheDirFor name version
      liftIO $ not <$> doesDirectoryExist (root </> "src")


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


verifyBuildArtifacts :: Map Name Version -> Task.Task RawInfo
verifyBuildArtifacts solution =
  do  Task.report (Progress.BuildDepsStart (Map.size solution))
      startMVar <- liftIO newEmptyMVar
      ifacesMVar <- liftIO $ newMVar Map.empty
      pkgInfoMVars <- Map.traverseWithKey (verifyBuild startMVar ifacesMVar) solution
      liftIO $ putMVar startMVar pkgInfoMVars
      answers <- liftIO $ traverse readMVar pkgInfoMVars
      Task.report Progress.BuildDepsEnd
      RawInfo
        <$> Map.traverseMaybeWithKey toInfo answers
        <*> liftIO (readMVar ifacesMVar)


data RawInfo =
  RawInfo (Map Name PkgInfo) Module.Interfaces


verifyBuild
  :: MVar (Map Name (MVar Answer))
  -> MVar Module.Interfaces
  -> Name
  -> Version
  -> Task.Task (MVar Answer)
verifyBuild pkgInfoMVar ifacesMVar name version =
  do  mvar <- liftIO newEmptyMVar
      info <- Get.info name version
      report <- Task.getReporter
      runner <- Task.getSilentRunner

      liftIO $ void $ forkIO $
        do  allMVars <- readMVar pkgInfoMVar
            let deps = Project._pkg_deps info
            let depsMVars = Map.intersection allMVars deps
            depAnswers <- traverse readMVar depsMVars

            answer <- ifNotBlocked depAnswers $ \infos ->
              do  ifaces <- readMVar ifacesMVar
                  let summary = Deps.makeCheapSummary info infos ifaces
                  either <- runner (build name version info summary)
                  case either of
                    Right ifaces ->
                      do  latest <- takeMVar ifacesMVar
                          putMVar ifacesMVar (Map.union latest ifaces)
                          return (Ok info)

                    Left _ ->
                      return (Err name version)

            report Progress.BuildDepsProgress
            putMVar mvar answer

      return mvar


build :: Name -> Version -> PkgInfo -> Deps.Summary -> Task.Task Module.Interfaces
build name version info summary =
  do  let project = Pkg info
      root <- Task.getPackageCacheDirFor name version
      graph <- Crawl.crawl root project summary
      (dirty, cachedIfaces) <- Plan.plan root project summary graph
      ifaces <- Compile.compileAll project cachedIfaces dirty
      Interface.write info ifaces
      return ifaces



-- ANSWERS


data Answer
  = Ok PkgInfo
  | Blocked
  | Err Name Version


toInfo :: Name -> Answer -> Task.Task (Maybe PkgInfo)
toInfo _ answer =
  case answer of
    Ok info ->
      return (Just info)

    Blocked ->
      return Nothing

    Err name version ->
      Task.throw (Error.BadDep name version)


ifNotBlocked :: Map Name Answer -> (Map Name PkgInfo -> IO Answer) -> IO Answer
ifNotBlocked answers callback =
  case traverse isOk answers of
    Nothing ->
      return Blocked

    Just infos ->
      callback infos


isOk :: Answer -> Maybe PkgInfo
isOk answer =
  case answer of
    Ok info -> Just info
    Blocked -> Nothing
    Err _ _ -> Nothing
