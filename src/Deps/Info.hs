module Deps.Info
  ( Info
  , ExposedModules
  , getExposedModules
  )
  where

import qualified Data.Map as Map

import Elm.Compiler.Module as Module
import Elm.Package (Name, Version)
import qualified Elm.Project as Project



data Info =
  Info
    { _deps :: [Project.PkgInfo]
    }


type ExposedModules =
  Map.Map Module.Raw [(Name, Version)]


getExposedModules :: Info -> ExposedModules
getExposedModules (Info _) =
  error "TODO"

