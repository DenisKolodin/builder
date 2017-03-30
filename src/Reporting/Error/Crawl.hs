{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Crawl
  ( Error(..)
  )
  where


import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg



-- ERRORS


data Error
  = NotFound (Maybe Module.Raw) -- TODO suggest other names
  | Duplicates [FilePath] [Pkg.Package]
  | BadHeader FilePath Compiler.Error
  | NoName FilePath Module.Raw
  | BadName FilePath Module.Raw
  | PortsInPackage FilePath
  | EffectsUnexpected FilePath

