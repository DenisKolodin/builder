{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Crawler
  ( Graph(..), Node(..)
  , crawlProject
  , Permissions(..)
  )
  where

import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (forM_)
import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import Elm.Project (Project)
import qualified Elm.Project as Project
import qualified File.IO as IO
import qualified Reporting.Error as Error
import qualified Reporting.Error.Crawler as E
import qualified Reporting.Task as Task
import qualified Stuff.Info as Stuff



-- GRAPH


data Graph =
  Graph
    { _locals :: Map.Map Module.Raw Node
    , _natives :: Map.Map Module.Raw FilePath
    , _foreigns :: Map.Map Module.Raw Pkg.Name
    , _problems :: Map.Map Module.Raw E.Problem
    }


data Node =
  Node
    { _path :: FilePath
    , _deps :: [Module.Raw]
    }


empty :: Graph
empty =
  Graph Map.empty Map.empty Map.empty Map.empty



-- CRAWL PROJECT


crawlProject :: FilePath -> Project -> Stuff.DepsInfo -> Permissions -> Task.Task Graph
crawlProject root project depsInfo permissions =
  let
    environment =
      Env
        { _srcDirs = [ root </> Project.toSourceDir project ]
        , _depModules = Stuff.getDepModules project depsInfo
        , _permissions = permissions
        , _native = Project.toNative project
      }
  in
    dfs environment $
      map (Unvisited Nothing) (Project.toRoots project)


data Env =
  Env
    { _srcDirs :: [FilePath]
    , _depModules :: Stuff.DepModules
    , _permissions :: Permissions
    , _native :: Bool
    }


data Permissions
  = PortsAndEffects
  | Effects
  | None



-- DEPTH FIRST SEARCH


dfs :: Env -> [Unvisited] -> Task.Task Graph
dfs env unvisited =
  do  (input, output) <- Task.pool (worker env)

      graph <-
        liftIO $
          do  mapM_ (writeChan input) unvisited
              dfsHelp env input output (length unvisited) empty

      if Map.null (_problems graph)
        then return graph
        else Task.throw (Error.Crawler (_problems graph))


dfsHelp :: Env -> Chan Unvisited -> Chan (Either E.Error Asset) -> Int -> Graph -> IO Graph
dfsHelp env input output pendingWork graph =
  if pendingWork == 0 then
    return graph

  else
    do  asset <- readChan output
        case asset of
          Right (Local name node) ->
            do  let locals = Map.insert name node (_locals graph)
                let newGraph = graph { _locals = locals }
                let deps = filter (isNew newGraph) (_deps node)
                forM_ deps $ \dep ->
                  writeChan input (Unvisited (Just name) dep)
                let newPending = pendingWork - 1 + length deps
                dfsHelp env input output newPending newGraph

          Right (Native name path) ->
            do  let natives = Map.insert name path (_natives graph)
                let newGraph = graph { _natives = natives }
                let newPending = pendingWork - 1
                dfsHelp env input output newPending newGraph

          Right (Foreign name pkg) ->
            do  let foreigns = Map.insert name pkg (_foreigns graph)
                let newGraph = graph { _foreigns = foreigns }
                let newPending = pendingWork - 1
                dfsHelp env input output newPending newGraph

          Left (E.Error name problem) ->
            do  let problems = Map.insert name problem (_problems graph)
                let newGraph = graph { _problems = problems }
                let newPending = pendingWork - 1
                dfsHelp env input output newPending newGraph


isNew :: Graph -> Module.Raw -> Bool
isNew (Graph locals natives foreigns problems) name =
  Map.notMember name locals
  && Map.notMember name natives
  && Map.notMember name foreigns
  && Map.notMember name problems



-- DFS WORKER


data Unvisited =
  Unvisited
    { _parent :: Maybe Module.Raw
    , _name :: Module.Raw
    }


data Asset
  = Local Module.Raw Node
  | Native Module.Raw FilePath
  | Foreign Module.Raw Pkg.Name


type CTask a = Task.Task_ E.Error a


worker :: Env -> Unvisited -> CTask Asset
worker env (Unvisited maybeParent name) =
  do
      filePaths <- liftIO $ find (_native env) name (_srcDirs env)

      case (filePaths, Map.lookup name (_depModules env)) of
        ([Elm filePath], Nothing) ->
            readValidHeader env name filePath

        ([JS filePath], Nothing) ->
            return (Native name filePath)

        ([], Just [(pkg, _vsn)]) ->
            return (Foreign name pkg)

        ([], Nothing) ->
            throw name (E.ModuleNotFound maybeParent)

        (_, maybePkgs) ->
          let
            locals = map toFilePath filePaths
            foreigns = maybe [] (map fst) maybePkgs
          in
            throw name (E.ModuleDuplicates maybeParent locals foreigns)


throw :: Module.Raw -> E.Problem -> CTask a
throw name problem =
  Task.throw (E.Error name problem)



-- FIND LOCAL FILE PATH


data CodePath = Elm FilePath | JS FilePath


toFilePath :: CodePath -> FilePath
toFilePath codePath =
  case codePath of
    Elm path ->
      path

    JS path ->
      path


find :: Bool -> Module.Raw -> [FilePath] -> IO [CodePath]
find allowsNative name srcDirs =
  do  elm <- mapM (elmExists name) srcDirs
      js <- if allowsNative then mapM (jsExists name) srcDirs else return []
      return (Maybe.catMaybes (js ++ elm))


elmExists :: Module.Raw -> FilePath -> IO (Maybe CodePath)
elmExists name srcDir =
  do  let path = srcDir </> Module.nameToPath name <.> "elm"
      exists <- doesFileExist path
      return $ if exists then Just (Elm path) else Nothing


jsExists :: Module.Raw -> FilePath -> IO (Maybe CodePath)
jsExists name srcDir =
  if not (Text.isPrefixOf "Native." name)
    then return Nothing
    else
      do  let path = srcDir </> Module.nameToPath name <.> "js"
          exists <- doesFileExist path
          return $ if exists then Just (JS path) else Nothing



-- READ DEPENDENCIES


readValidHeader :: Env -> Module.Raw -> FilePath -> CTask Asset
readValidHeader env expectedName filePath =
  do  source <- liftIO (IO.readUtf8 filePath)

      (tag, name, deps) <-
        case Compiler.parseDependencies (error "TODO") source of
          Right result ->
            return result

          Left msg ->
            throw expectedName (E.BadHeader filePath msg)

      checkName filePath expectedName name
      checkTag filePath name (_permissions env) tag

      return (Local name (Node filePath deps))


checkName :: FilePath -> Module.Raw -> Module.Raw -> CTask ()
checkName path expectedName actualName =
  if expectedName == actualName then
    return ()

  else
    throw expectedName (E.ModuleNameMismatch path actualName)


checkTag :: FilePath -> Module.Raw -> Permissions -> Compiler.Tag -> CTask ()
checkTag filePath name permissions tag =
  case (permissions, tag) of
    (PortsAndEffects, _) ->
      return ()

    (_, Compiler.Port) ->
      throw name (E.UnpublishablePorts filePath)

    (None, Compiler.Effect)  ->
      throw name (E.UnpublishableEffects filePath)

    (_, _) ->
      return ()
