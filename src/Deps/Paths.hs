module Deps.Paths
  ( elmi
  , elmo
  )
  where


import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg



-- ELMI and ELMO


elmi :: FilePath -> Pkg.Name -> Pkg.Version -> Module.Raw -> FilePath
elmi root pkg vsn name =
  toArtifactPath root pkg vsn name "elmi"


elmo :: FilePath -> Pkg.Name -> Pkg.Version -> Module.Raw -> FilePath
elmo root pkg vsn name =
  toArtifactPath root pkg vsn name "elmo"


toArtifactPath :: FilePath -> Pkg.Name -> Pkg.Version -> Module.Raw -> String -> FilePath
toArtifactPath root pkg vsn name ext =
  root
  </> Pkg.toFilePath pkg
  </> Pkg.versionToString vsn
  </> Text.unpack (Module.hyphenate name)
  <.> ext
