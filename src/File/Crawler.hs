{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Crawler where

import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Package)

import qualified Deps.Info as Deps
import Elm.Project (Project)
import qualified Elm.Project as Project
import qualified File.IO as File
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task



-- GRAPH


data Graph =
  Graph
    { _modules :: Map.Map Module.Raw Node
    , _natives :: Map.Map Module.Raw FilePath
    , _foreigns :: Map.Map Module.Raw Package
    }


data Node =
  Node
    { _path :: FilePath
    , _deps :: [Module.Raw]
    }



-- STATE and ENVIRONMENT


data Env =
  Env
    { _srcDirs :: [FilePath]
    , _depModules :: Deps.ExposedModules
    , _permissions :: Permissions
    , _pkgName :: Maybe Name
    , _native :: Bool
    }


data Permissions
  = PortsAndEffects
  | Effects
  | None


initEnv :: FilePath -> Project -> Deps.Info -> Permissions -> Env
initEnv root project depsInfo permissions =
  Env
    [ root </> Project.toSourceDir project ]
    (Deps.getExposedModules depsInfo)
    permissions
    (Project.toName project)
    (Project.toNative project)



{-- GENERIC CRAWLER


dfsFromFiles
  :: FilePath
  -> Solution.Solution
  -> Desc.Description
  -> Permissions
  -> [FilePath]
  -> Task.Task ([Module.Raw], Graph)
dfsFromFiles root solution desc permissions filePaths =
  do  env <- initEnv root desc solution permissions

      info <- mapM (readDeps env Nothing) filePaths
      let names = map fst info
      let unvisited = concatMap (snd . snd) info
      let pkgData = Map.fromList (map (second fst) info)
      let initialGraph = Graph pkgData Map.empty Map.empty

      summary <- dfs env unvisited initialGraph

      return (names, summary)


dfsFromExposedModules
  :: FilePath
  -> Solution.Solution
  -> Desc.Description
  -> Permissions
  -> Task.Task Graph
dfsFromExposedModules root solution desc permissions =
  do  env <- initEnv root desc solution permissions
      let unvisited = map (Unvisited Nothing) (Desc.exposed desc)
      let summary = Graph Map.empty Map.empty Map.empty
      dfs env unvisited summary

--}



-- DEPTH FIRST SEARCH


data Unvisited =
  Unvisited
    { _parent :: Maybe Module.Raw
    , _name :: Module.Raw
    }


dfs :: Env -> [Unvisited] -> Graph -> Task.Task Graph
dfs env unvisited graph =
  case unvisited of
    [] ->
      return graph

    next : rest ->
      if Map.member (_name next) (_modules graph) then
        dfs env rest graph

      else
        dfsHelp env next rest graph


dfsHelp :: Env -> Unvisited -> [Unvisited] -> Graph -> Task.Task Graph
dfsHelp env (Unvisited maybeParent name) unvisited graph =
  do  -- find all paths that match the unvisited module name
      filePaths <- find (_native env) name (_srcDirs env)

      -- see if we found a unique path for the name
      case (filePaths, Map.lookup name (_depModules env)) of
        ([Elm filePath], Nothing) ->
          do  (_, node, newUnvisited) <- readDeps env (Just name) filePath

              dfs env (newUnvisited ++ unvisited) $
                graph { _modules = Map.insert name node (_modules graph) }

        ([JS filePath], Nothing) ->
            dfs env unvisited $
              graph { _natives = Map.insert name filePath (_natives graph) }

        ([], Just [pkg]) ->
            dfs env unvisited $
              graph { _foreigns = Map.insert name pkg (_foreigns graph) }

        ([], Nothing) ->
            throw (E.ModuleNotFound name maybeParent)

        (_, maybePkgs) ->
          let
            locals = map toFilePath filePaths
            foreigns = maybe [] (map fst) maybePkgs
          in
            throw $ E.ModuleDuplicates name maybeParent locals foreigns


throw :: E.Error -> Task.Task a
throw assetsError =
  Task.throw (Error.Assets assetsError)



-- FIND LOCAL FILE PATH


data CodePath = Elm FilePath | JS FilePath


toFilePath :: CodePath -> FilePath
toFilePath codePath =
  case codePath of
    Elm file -> file
    JS file -> file


find :: Bool -> Module.Raw -> [FilePath] -> Task.Task [CodePath]
find allowsNative moduleName sourceDirs =
    findHelp allowsNative [] moduleName sourceDirs


findHelp :: Bool -> [CodePath] -> Module.Raw -> [FilePath] -> Task.Task [CodePath]
findHelp _allowsNative locations _moduleName [] =
  return locations

findHelp allowsNative locations moduleName (dir:srcDirs) =
  do  locations' <- addElmPath locations
      updatedLocations <-
          if allowsNative then addJsPath locations' else return locations'
      findHelp allowsNative updatedLocations moduleName srcDirs
  where
    consIf bool x xs =
        if bool then x:xs else xs

    addElmPath locs =
      do  let elmPath = dir </> Module.nameToPath moduleName <.> "elm"
          elmExists <- liftIO (doesFileExist elmPath)
          return (consIf elmExists (Elm elmPath) locs)

    addJsPath locs =
      do  let jsPath = dir </> Module.nameToPath moduleName <.> "js"
          jsExists <-
              if Text.isPrefixOf "Native." moduleName then
                liftIO (doesFileExist jsPath)
              else
                return False

          return (consIf jsExists (JS jsPath) locs)



-- READ DEPENDENCIES


readDeps
  :: Env
  -> Maybe Module.Raw
  -> FilePath
  -> Task.Task (Module.Raw, Node, [Unvisited])
readDeps env maybeName filePath =
  do  source <- liftIO (File.readUtf8 filePath)

      (tag, name, deps) <-
        case Compiler.parseDependencies (_pkgName env) source of
          Right result ->
            return result

          Left msg ->
            Task.throw (Error.BadCompile filePath source [msg])

      checkName filePath name maybeName
      checkTag filePath name (_permissions env) tag

      return
        ( name
        , Node filePath deps
        , map (Unvisited (Just name)) deps
        )


checkName :: FilePath -> Module.Raw -> Maybe Module.Raw -> Task.Task ()
checkName path nameFromSource maybeName =
  case maybeName of
    Just nameFromPath | nameFromSource /= nameFromPath ->
      throw (E.ModuleNameMismatch path nameFromPath nameFromSource)

    _ ->
      return ()


checkTag :: FilePath -> Module.Raw -> Permissions -> Compiler.Tag -> Task.Task ()
checkTag filePath name permissions tag =
  case (permissions, tag) of
    (PortsAndEffects, _) ->
      return ()

    (_, Compiler.Port) ->
      throw (E.UnpublishablePorts filePath name)

    (None, Compiler.Effect)  ->
      throw (E.UnpublishableEffects filePath name)

    (_, _) ->
      return ()
