module Stuff.Paths
  ( solution
  , summary
  , removeStuff
  , elmi
  , elmo
  , temp
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.Text as Text
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Reporting.Task as Task



-- PATHS


solution :: FilePath
solution =
  stuff </> "solution.dat"


summary :: FilePath
summary =
  stuff </> "summary.dat"


stuff :: FilePath
stuff =
  "elm-stuff" </> Pkg.versionToString Compiler.version



-- REMOVE STUFF


removeStuff :: FilePath -> Task.Task_ e ()
removeStuff root =
  liftIO $
  do  let dir = root </> "elm-stuff"
      exists <- Dir.doesDirectoryExist dir
      if exists
        then Dir.removeDirectoryRecursive dir
        else return ()



-- ELMI and ELMO


elmi :: Module.Raw -> FilePath
elmi name =
  toArtifactPath name "elmi"


elmo :: Module.Raw -> FilePath
elmo name =
  toArtifactPath name "elmo"


toArtifactPath :: Module.Raw -> String -> FilePath
toArtifactPath name ext =
  stuff </> Text.unpack (Module.hyphenate name) <.> ext



-- TEMP


temp :: FilePath
temp =
  stuff </> "temp.js"
