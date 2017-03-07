module Stuff.Paths
  ( solution
  , summary
  , elmi
  , elmo
  )
  where


import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg



-- PATHS


stuff :: FilePath
stuff =
  "elm-stuff" </> Pkg.versionToString Compiler.version


solution :: FilePath
solution =
  stuff </> "solution.dat"


summary :: FilePath
summary =
  stuff </> "summary.dat"



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

