{-# OPTIONS_GHC -Wall #-}
module Elm.Assets
  ( projectPath
  , getPackageRoot
  , getReplRoot
  , getMakeRoot
  )
  where

import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath ((</>))

import qualified Elm.Compiler.Version as Version
import qualified Elm.Package as Pkg



-- PATHS


projectPath :: FilePath
projectPath =
  "elm.json"



-- GET ROOTS


getPackageRoot :: IO FilePath
getPackageRoot =
  getRoot "package"


getReplRoot :: IO FilePath
getReplRoot =
  getRoot "repl"


getMakeRoot :: IO FilePath
getMakeRoot =
  getRoot "make"


getRoot :: FilePath -> IO FilePath
getRoot projectName =
  do  maybeHome <- Env.lookupEnv "ELM_HOME"
      home <- maybe (Dir.getAppUserDataDirectory "elm") return maybeHome
      let root = home </> version </> projectName
      Dir.createDirectoryIfMissing True root
      return root


version :: FilePath
version =
  Pkg.versionToString Version.version
