{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Find
  ( Asset(..)
  , find
  )
  where

import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import Elm.Project (Project)
import qualified Elm.Project as Project
import qualified Reporting.Error.Find as E
import qualified Reporting.Task as Task
import qualified Stuff.Info as Info



-- ASSET


data Asset
  = Local FilePath
  | Native FilePath
  | Foreign Pkg.Name



-- FIND


find :: FilePath -> Project -> Info.DepModules -> Module.Raw -> Task.Task_ E.Error Asset
find root project depModules name =
  do
      codePaths <- liftIO $ getCodePaths root project name

      case (codePaths, Map.lookup name depModules) of
        ([Elm path], Nothing) ->
            return (Local path)

        ([JS path], Nothing) ->
            return (Native path)

        ([], Just [(pkg, _vsn)]) ->
            return (Foreign pkg)

        ([], Nothing) ->
            Task.throw E.NotFound

        (_, maybePkgs) ->
          let
            locals = map toFilePath codePaths
            foreigns = maybe [] (map fst) maybePkgs
          in
            Task.throw (E.Duplicates locals foreigns)



-- CODE PATH


data CodePath
  = Elm FilePath
  | JS FilePath


toFilePath :: CodePath -> FilePath
toFilePath codePath =
  case codePath of
    Elm path ->
      path

    JS path ->
      path



-- GET CODE PATHS


getCodePaths :: FilePath -> Project -> Module.Raw -> IO [CodePath]
getCodePaths root project name =
  do  let srcDirs = [root </> Project.getSourceDir project]
      let allowNative = Project.getNative project
      elm <- mapM (elmExists name) srcDirs
      Maybe.catMaybes <$>
        if allowNative && Text.isPrefixOf "Native." name then
          (++ elm) <$> mapM (jsExists name) srcDirs
        else
          return elm


elmExists :: Module.Raw -> FilePath -> IO (Maybe CodePath)
elmExists name srcDir =
  do  let path = srcDir </> Module.nameToPath name <.> "elm"
      exists <- doesFileExist path
      return $ if exists then Just (Elm path) else Nothing


jsExists :: Module.Raw -> FilePath -> IO (Maybe CodePath)
jsExists name srcDir =
  do  let path = srcDir </> Module.nameToPath name <.> "js"
      exists <- doesFileExist path
      return $ if exists then Just (JS path) else Nothing

