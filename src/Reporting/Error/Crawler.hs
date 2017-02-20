{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Crawler
  ( Error(..)
  , Problem(..)
  )
  where


import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg



-- ERRORS


data Error
  = Error Module.Raw Problem


-- TODO show source code when appropriate
-- Need Elm.Compiler.parseDependencies to produce regions.


data Problem
  = ModuleNotFound (Maybe Module.Raw) -- TODO suggest other names
  | ModuleDuplicates (Maybe Module.Raw) [FilePath] [Pkg.Name]
  | ModuleNameMismatch FilePath Module.Raw
  | UnpublishablePorts FilePath
  | UnpublishableEffects FilePath
  | BadHeader FilePath Compiler.Error

