{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Crawl
  ( Error(..)
  , toString
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


toString :: Error -> String
toString err =
  case err of
    NotFound maybeParent ->
      "NotFound " ++ show maybeParent

    Duplicates paths pkgs ->
      "Duplicates " ++ show paths

    BadHeader path _ ->
      "BadHeader " ++ show path

    NoName path name ->
      "NoName " ++ show (path, name)

    BadName path name ->
      "BadName " ++ show (path, name)

    PortsInPackage path ->
      "PortsInPackage " ++ show path

    EffectsUnexpected path ->
      "EffectsUnexpected " ++ show path
