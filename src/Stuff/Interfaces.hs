{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
module Stuff.Interfaces
  ( Interfaces
  , load
  )
  where

import Control.Monad (foldM)
import Control.Monad.Except (catchError)
import Data.Binary (Binary)
import qualified Data.Map as Map
import GHC.Generics (Generic)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified File.IO as IO
import qualified Reporting.Task as Task
import qualified Stuff.Validate as Stuff
import qualified Stuff.Paths as Path



-- INTERFACES


type Interfaces =
  Map.Map CanonicalModule Module.Interface


data CanonicalModule =
  CanonicalModule
    { _pkg :: Pkg.Package
    , _name :: Module.Raw
    }
    deriving (Eq, Ord, Generic)


instance Binary CanonicalModule



-- LOAD


load :: Stuff.DepsInfo -> Task.Task Interfaces
load depsInfo =
  IO.readBinary Path.ifaces
    `catchError` (\_ -> rebuild depsInfo)



-- REBUILD


rebuild :: Stuff.DepsInfo -> Task.Task Interfaces
rebuild depsInfo =
  let
    packages =
      Stuff.getPackages depsInfo
  in
    do  ifaces <- foldM rebuildHelp Map.empty packages
        IO.writeBinary Path.ifaces ifaces
        return ifaces


rebuildHelp :: Interfaces -> Pkg.Package -> Task.Task Interfaces
rebuildHelp ifaces (name, version) =
  error "TODO" ifaces name version
