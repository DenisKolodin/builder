{-# OPTIONS_GHC -Wall #-}
module Elm.Project.Summary
  ( Summary(..)
  , ExposedModules
  , init
  , cheapInit
  )
  where


import Prelude hiding (init)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)
import Elm.Project.Json (Project(..), AppInfo(..), PkgInfo(..))
import qualified Elm.Project.Json as Project



-- SUMMARY


data Summary =
  Summary
    { _root :: FilePath
    , _project :: Project
    , _exposed :: ExposedModules
    , _ifaces :: Module.Interfaces
    }


type ExposedModules =
  Map.Map Module.Raw [(Name, Version)]



-- MAKE SUMMARY


init :: FilePath -> Project -> Map Name PkgInfo -> Module.Interfaces -> Summary
init root project deps ifaces =
  let
    exposed =
      Project.get
        (getExposed deps . _app_deps)
        (getExposed deps . _pkg_deps)
        project

    privatizedInterfaces =
      Map.mapMaybeWithKey (privatize exposed) ifaces
  in
    Summary root project exposed privatizedInterfaces


privatize :: ExposedModules -> Module.Canonical -> Module.Interface -> Maybe Module.Interface
privatize exposed (Module.Canonical _ name) iface =
  case Map.lookup name exposed of
    Just [_] ->
      Just iface

    _ ->
      Module.privatize iface



-- MAKE CHEAP SUMMARY


cheapInit :: FilePath -> PkgInfo -> Map Name PkgInfo -> Module.Interfaces -> Summary
cheapInit root info deps ifaces =
  Summary root (Pkg info) (getExposed deps (_pkg_deps info)) ifaces


getExposed :: Map Name PkgInfo -> Map Name a -> ExposedModules
getExposed deps directs =
  Map.foldl insertExposed Map.empty (Map.intersection deps directs)


insertExposed :: ExposedModules -> PkgInfo -> ExposedModules
insertExposed exposed info =
  let
    home =
      ( _pkg_name info, _pkg_version info )

    insertModule dict modul =
      Map.insertWith (++) modul [home] dict
  in
    List.foldl' insertModule exposed (_pkg_exposed info)
