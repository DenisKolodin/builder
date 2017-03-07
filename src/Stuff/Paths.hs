module Stuff.Paths
  ( solution
  , exposed
  , ifaces
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


exposed :: FilePath
exposed =
  stuff </> "exposed.dat"


ifaces :: FilePath
ifaces =
  stuff </> "ifaces.dat"



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

