{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Assets
  ( Error(..)
  , JsonProblem(..)
  , toDoc
  )
  where

import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<+>), (<>), dullyellow, fillSep, green, indent, text, vcat )

import qualified Elm.Package as Pkg

import qualified Json.Decode as Decode
import qualified Reporting.Error.Help as Help
import Reporting.Error.Help (reflow)



-- ERRORS


data Error
  = BadElmJson JsonProblem
  | BadBuildPlan JsonProblem
  | CorruptElmJson Pkg.Name Pkg.Version
  | CorruptDocumentation String
  | CorruptVersionCache Pkg.Name
  | PackageNotFound Pkg.Name [Pkg.Name]
  | CorruptBinary FilePath


data JsonProblem
  = BadSyntax
  | BadStructure Decode.Error
  | BadContent



-- TO DOC


toDoc :: Error -> Doc
toDoc err =
  case err of
    BadElmJson problem ->
      case problem of
        BadSyntax ->
          Help.makeErrorDoc
            ("Your elm.json is not valid JSON.")
            [ reflow "Maybe a comma is missing? Or a closing } or ]?"
            ]

        BadStructure jsonError ->
          let
            (location, json, expecting) =
              getJsonErrorInfo jsonError "project"
          in
            Help.makeErrorDoc ("Problem in your elm.json file.") $
              if location == "project" then
                [ text "I was expecting" <+> green (text expecting) <> text "."
                ]

              else
                [ fillSep $
                    text "I was expecting"
                    : map (green . text) (words expecting)
                    ++
                      [ text "at"
                      , dullyellow (text location)
                      ]
                ]

        BadContent ->
          error "TODO bad json bad content"

    CorruptDocumentation problem ->
      Help.makeErrorDoc "CorruptDocumentation" [ text "TODO" ]

    CorruptVersionCache name ->
      Help.makeErrorDoc "CorruptVersionCache" [ text "TODO" ]

    PackageNotFound package suggestions ->
      Help.makeErrorDoc
        ( "Could not find any packages named " ++ Pkg.toString package
        )
        [ text $ "Maybe you want one of these instead?"
        , indent 4 $ vcat $ map (text . Pkg.toString) suggestions
        ]

    CorruptBinary path ->
      Help.makeErrorDoc "CorruptBinary" [ text $ "TODO " ++ path ]


getJsonErrorInfo :: Decode.Error -> String -> (String, Aeson.Value, String)
getJsonErrorInfo err location =
  case err of
    Decode.Field field subErr ->
      getJsonErrorInfo subErr $
        if Text.isInfixOf "-" field then
          location ++ "[\"" ++ Text.unpack field ++ "\"]"

        else
          location ++ "." ++ Text.unpack field

    Decode.Index index subErr ->
      getJsonErrorInfo subErr $
        location ++ "[" ++ show index ++ "]"

    Decode.Failure value expecting ->
      (location, value, expecting)
