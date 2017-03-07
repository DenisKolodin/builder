{-# OPTIONS_GHC -Wall #-}
module Deps.Interface
  ( read
  , write
  )
  where

import Prelude hiding (read)
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import Elm.Project (PkgInfo)
import qualified Elm.Project as Project
import qualified File.IO as IO
import qualified Reporting.Task as Task



-- READ


read :: Name -> Version -> Task.Task Module.Interfaces
read name version =
  IO.readBinary =<< datPath name version


datPath :: Name -> Version -> Task.Task FilePath
datPath name version =
  do  dir <- Task.getPackageCacheDirFor name version
      return (dir </> "ifaces.dat")



-- WRITE


write :: PkgInfo -> Module.Interfaces -> Task.Task ()
write info interfaces =
  do  let name = Project._pkg_name info
      let version = Project._pkg_version info
      path <- datPath name version
      IO.writeBinary path (crush info interfaces)


crush :: PkgInfo -> Module.Interfaces -> Module.Interfaces
crush info interfaces =
  let
    name =
      Project._pkg_name info

    exposed =
      Set.fromList (Project._pkg_exposed info)
  in
    Map.mapMaybeWithKey (crushHelp name exposed) interfaces


crushHelp :: Name -> Set.Set Module.Raw -> Module.Canonical -> Module.Interface -> Maybe Module.Interface
crushHelp target exposed (Module.Canonical pkg name) iface =
  if target /= pkg then
    Nothing

  else if Set.member name exposed then
    Just iface

  else
    Module.privatize iface

