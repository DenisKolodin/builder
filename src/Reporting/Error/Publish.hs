{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Publish
  ( Error(..)
  , toDoc
  )
  where

import qualified Data.List as List
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Package as Pkg
import qualified Reporting.Error.Help as Help



-- ERRORS


data Error
  = Application
  | NoSummary
  | NoExposed
  | NoReadme
  | ShortReadme
  | NoLicense
  | NoGit
  | LocalChanges



-- TO DOC


toDoc :: Error -> P.Doc
toDoc err =
  case err of
    Application ->
      Help.makeErrorDoc "I cannot publish applications, only packages!" []

    NoSummary ->
      error "TODO NoSummary"

    NoExposed ->
      error "TODO NoExposed"

    NoReadme ->
      error "TODO NoReadme"

    ShortReadme ->
      error "TODO ShortReadme"

    NoLicense ->
      error "TODO NoLicense"

    NoGit ->
      error "TODO NoGit"

    LocalChanges ->
      error "TODO LocalChanges"

