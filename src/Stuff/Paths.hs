module Stuff.Paths
  ( pkgInfo
  , deps
  , elmi
  , elmo
  )
  where


import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler.Module as Module



-- PATHS


stuff :: FilePath
stuff =
  "elm-stuff"


pkgInfo :: FilePath
pkgInfo =
  stuff </> "elm.dat"


deps :: FilePath
deps =
  stuff </> "deps.dat"



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

