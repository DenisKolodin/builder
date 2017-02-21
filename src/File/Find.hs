{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Find
  ( CodePath(..)
  , toFilePath
  , find
  )
  where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler.Module as Module



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



-- FIND


find :: Bool -> Module.Raw -> [FilePath] -> IO [CodePath]
find allowsNative name srcDirs =
  do  elm <- mapM (elmExists name) srcDirs
      Maybe.catMaybes <$>
        if allowsNative && Text.isPrefixOf "Native." name then
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

