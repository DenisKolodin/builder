{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Bump
  ( Error(..)
  , toReport
  )
  where

import qualified Data.List as List
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Package as Pkg
import qualified Reporting.Error.Help as Help



-- ERRORS


data Error
  = Application
  | Unbumpable Pkg.Version [Pkg.Version]



-- TO REPORT


toReport :: Error -> Help.Report
toReport err =
  case err of
    Application ->
      error "TODO Application"

    Unbumpable vsn versions ->
      let
        list =
          case map Pkg.versionToString versions of
            [v] ->
              " to " ++ v ++ "."

            [v,w] ->
              " to " ++ v ++ " or " ++ w ++ "."

            vsnStrings ->
              " to one of these:  "++ List.intercalate ", " vsnStrings
      in
        error "TODO" Nothing
          ( "To compute a version bump, I need to start with a version that has\
            \ already been published. Your elm.json says I should start with version "
            ++ Pkg.versionToString vsn
            ++ ", but I cannot find that version on <http://package.elm-lang.org>."
          )
          [ Help.reflow $
              "Try again after changing the version in elm.json" ++ list
          ]
