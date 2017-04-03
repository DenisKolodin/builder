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

import Elm.Project.Json (Project)
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Error.Crawl as E
import qualified Reporting.Task as Task



-- ASSET


data Asset
  = Local FilePath  -- TODO carry source code to avoid 2nd read?
  | Kernel FilePath
  | Foreign Pkg.Package



-- FIND


find :: Summary.Summary -> Maybe Module.Raw -> Module.Raw -> Task.Task_ E.Error Asset
find (Summary.Summary root project exposed _ _) parent name =
  do
      codePaths <- liftIO $ getCodePaths root project name

      case (codePaths, Map.lookup name exposed) of
        ([Elm path], Nothing) ->
            return (Local path)

        ([JS path], Nothing) ->
            return (Kernel path)

        ([], Just [pkg]) ->
            return (Foreign pkg)

        ([], Nothing) ->
            Task.throw (E.NotFound parent)

        (_, maybePkgs) ->
          let
            locals = map toFilePath codePaths
            foreigns = maybe [] id maybePkgs
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
  do  let srcDirs = map (root </>) (Project.getSourceDirs project)
      elm <- mapM (elmExists name) srcDirs
      Maybe.catMaybes <$>
        if Text.isPrefixOf "Elm.Kernel." name && Project.isKernel project then
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
