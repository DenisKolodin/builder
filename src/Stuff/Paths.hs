module Stuff.Paths
  ( dir
  , pkgInfo
  , deps
  , elmi
  , elmo
  )
  where


import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))

import Elm.Compiler.Module as Module



-- PATHS


dir :: FilePath
dir =
  "elm-stuff"


pkgInfo :: FilePath
pkgInfo =
  dir </> "elm.dat"


deps :: FilePath
deps =
  dir </> "deps.dat"



-- ELMI and ELMO


elmi :: Module.Raw -> FilePath
elmi name =
  dir </> Text.unpack (Module.hyphenate name) <.> "elmi"


elmo :: Module.Raw -> FilePath
elmo name =
  dir </> Text.unpack (Module.hyphenate name) <.> "elmo"
