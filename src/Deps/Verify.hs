module Deps.Verify (verify) where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (filterM, forM, void)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Docs as Docs
import Elm.Package (Name, Version)
import Elm.Project.Json (Project(..), AppInfo(..), PkgInfo(..))
import qualified Json.Encode as Encode

import qualified Deps.Explorer as Explorer
import qualified Deps.Get as Get
import qualified Deps.Solver as Solver
import qualified Deps.Website as Website
import qualified Elm.Compiler as Compiler
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Summary as Summary
import qualified File.Args as Args
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.IO as IO
import qualified File.Plan as Plan
import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- VERIFY


verify :: FilePath -> Project -> Task.Task (Map Name Version, Summary.Summary)
verify root project =
  do  solution <- Project.get verifyApp verifyPkg project
      (RawInfo infos ifaces) <- verifyArtifacts solution
      let summary = Summary.init root project infos ifaces
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
  do  Website.download =<< filterM noSrc (Map.toList solution)
      verifyBuildArtifacts solution


noSrc :: (Name, Version) -> Task.Task Bool
noSrc (name, version) =
  do  root <- Task.getPackageCacheDirFor name version
      liftIO $ not <$> doesDirectoryExist (root </> "src")



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
                  either <- runner (getIface name version info infos ifaces)
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



-- GET INTERFACE


getIface
  :: Name
  -> Version
  -> PkgInfo
  -> Map Name PkgInfo
  -> Module.Interfaces
  -> Task.Task Module.Interfaces
getIface name version info infos depIfaces =
  do  root <- Task.getPackageCacheDirFor name version
      let solution = Map.map _pkg_version infos

      cached <- isCached root solution

      if cached
        then IO.readBinary (root </> "ifaces.dat")
        else
          do  Paths.removeStuff root

              let summary = Summary.cheapInit root info infos depIfaces
              args <- Args.fromSummary summary
              graph <- Crawl.crawl summary args
              (dirty, cachedIfaces) <- Plan.plan summary graph
              answers <- Compile.compile (Pkg info) cachedIfaces dirty
              results <- Artifacts.ignore answers

              Paths.removeStuff root

              updateCache root name info solution graph results



-- IS CACHED?


isCached :: FilePath -> Map Name Version -> Task.Task Bool
isCached root solution =
  IO.andM
    [ IO.exists (root </> "cached.dat")
    , IO.exists (root </> "ifaces.dat")
    , IO.exists (root </> "objs.dat")
    , IO.exists (root </> "documentation.json")
    , isCachedHelp solution <$> IO.readBinary (root </> "cached.dat")
    ]


isCachedHelp :: Map Name Version -> Map Name (Set Version) -> Bool
isCachedHelp solution cachedDeps =
  let
    matches =
      Map.intersectionWith Set.member solution cachedDeps
  in
    Map.size solution == Map.size matches
    && Map.foldr (&&) True matches



-- UPDATE CACHE


updateCache
  :: FilePath
  -> Name
  -> PkgInfo
  -> Map Name Version
  -> Crawl.Graph ()
  -> Map Module.Raw Compiler.Result
  -> Task.Task Module.Interfaces
updateCache root name info solution graph results =
  do  let path = root </> "cached.dat"
      let deps = Map.map Set.singleton solution
      let ifaces = crush name info results

      exists <- IO.exists path

      if exists
        then
          do  oldDeps <- IO.readBinary path
              IO.writeBinary path (Map.unionWith Set.union deps oldDeps)
        else
          do  IO.writeBinary (root </> "ifaces.dat") ifaces
              IO.writeBinary path deps
              let resultList = Map.elems results
              let objs = Obj.graphForPackage (Crawl._kernels graph) resultList
              IO.writeBinary (root </> "objs.dat") objs

              let docs = Maybe.mapMaybe Compiler._docs resultList
              let json = Encode.list Docs.encode docs
              liftIO $ Encode.write (root </> "documentation.json") json

      return ifaces



-- CRUSH INTERFACES


crush :: Name -> PkgInfo -> Map Module.Raw Compiler.Result -> Module.Interfaces
crush pkg info results =
  let
    exposed =
      Set.fromList (Project._pkg_exposed info)
  in
    Map.mapKeys (Module.Canonical pkg) $
      Map.mapMaybeWithKey (crushHelp exposed) results


crushHelp :: Set Module.Raw -> Module.Raw -> Compiler.Result -> Maybe Module.Interface
crushHelp exposed name (Compiler.Result _ iface _) =
  if Set.member name exposed then
    Just iface

  else
    Module.privatize iface
