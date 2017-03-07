{-# OPTIONS_GHC -Wall #-}
module Stuff.Deps
  ( Summary(..)
  , ExposedModules
  , makeSummary
  , makeCheapSummary
  )
  where


import Data.Binary (Binary, get, put)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)
import Elm.Project (Project(..), AppInfo(..), PkgInfo(..))
import qualified Elm.Project as Project



-- SUMMARY


data Summary =
  Summary
    { _exposed :: ExposedModules
    , _ifaces :: Module.Interfaces
    }


type ExposedModules =
  Map.Map Module.Raw [(Name, Version)]



-- BINARY


instance Binary Summary where
  get =
    Summary <$> get <*> get

  put (Summary exposed ifaces) =
    do  put exposed
        put ifaces



-- MAKE SUMMARY


makeSummary :: Project -> Map Name PkgInfo -> Module.Interfaces -> Summary
makeSummary project deps ifaces =
  let
    exposed =
      Project.get
        (getExposed deps . _app_deps)
        (getExposed deps . _pkg_deps)
        project
  in
    Summary exposed (Map.mapMaybeWithKey (privatize exposed) ifaces)


privatize :: ExposedModules -> Module.Canonical -> Module.Interface -> Maybe Module.Interface
privatize exposed (Module.Canonical _ name) iface =
  case Map.lookup name exposed of
    Just [_] ->
      Just iface

    _ ->
      Module.privatize iface



-- MAKE CHEAP SUMMARY


makeCheapSummary :: PkgInfo -> Map Name PkgInfo -> Module.Interfaces -> Summary
makeCheapSummary info deps ifaces =
  Summary (getExposed deps (_pkg_deps info)) ifaces


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
