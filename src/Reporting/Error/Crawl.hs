{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
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
      "TODO NotFound " ++ show maybeParent

    Duplicates paths pkgs ->
      "TODO Duplicates " ++ show paths

    BadHeader path err ->
      "TODO BadHeader\n\n" ++ show (Compiler.errorToDoc Compiler.dummyLocalizer path "1234567890123456789012345678901234567890" err)

    NoName path name ->
      "TODO NoName " ++ show (path, name)

    BadName path name ->
      "TODO BadName " ++ show (path, name)

    PortsInPackage path ->
      "TODO PortsInPackage " ++ path

    EffectsUnexpected path ->
      "TODO EffectsUnexpected " ++ path
