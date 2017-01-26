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



-- INFO


data Info =
  Info
    { _deps :: [Project.PkgInfo]
    }



-- EXPOSED MODULES


type ExposedModules =
  Map.Map Module.Raw [(Name, Version)]


getExposedModules :: Info -> ExposedModules
getExposedModules (Info deps) =
  foldr insertPkg Map.empty deps


insertPkg :: Project.PkgInfo -> ExposedModules -> ExposedModules
insertPkg info exposedModules =
  let
    home =
      ( Project.toPkgName info
      , Project._pkg_version info
      )

    insertModule modul dict =
      Map.insertWith (++) modul [home] dict
  in
    foldr insertModule exposedModules (Project._pkg_exposed info)
