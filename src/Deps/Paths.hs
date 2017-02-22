module Deps.Paths
  ( elmi
  , elmo
  )
  where


import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import Reporting.Task (Task_)
import qualified Reporting.Task as Task



-- ELMI and ELMO


elmi :: Pkg.Name -> Pkg.Version -> Module.Raw -> Task_ e FilePath
elmi pkg vsn name =
  toArtifactPath pkg vsn name "elmi"


elmo :: Pkg.Name -> Pkg.Version -> Module.Raw -> Task_ e FilePath
elmo pkg vsn name =
  toArtifactPath pkg vsn name "elmo"


toArtifactPath :: Pkg.Name -> Pkg.Version -> Module.Raw -> String -> Task_ e FilePath
toArtifactPath pkg vsn name ext =
  do  root <- Task.getPackageCacheDirFor pkg vsn
      let path = Text.unpack (Module.hyphenate name)
      return (root </> path <.> ext)
