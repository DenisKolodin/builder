{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Assets
  ( Error(..)
  , toDoc
  )
  where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<+>), (<>), dullyellow, fillSep, green, indent, text, vcat )

import qualified Elm.Package as Pkg

import qualified Utils.Json as Json
import qualified Reporting.Error.Help as Help
import Reporting.Error.Help (reflow)



-- ERRORS


data Error
  = BadElmJson FilePath (Maybe Json.Error)
  | CorruptDocumentation String
  | CorruptVersionCache Pkg.Name
  | PackageNotFound Pkg.Name [Pkg.Name]
  | CorruptBinary FilePath



-- TO DOC


toDoc :: Error -> Doc
toDoc err =
  case err of
    BadElmJson path maybeProblem ->
      case maybeProblem of
        Nothing ->
          Help.makeErrorDoc
            ("Looks like the JSON in " ++ path ++ " is messed up.")
            [ reflow "Maybe a comma is missing? Or a closing } or ]?"
            ]

        Just jsonError ->
          let
            (location, json, expecting) =
              getJsonErrorInfo jsonError "project"
          in
            Help.makeErrorDoc ("Something is wrong in your " ++ path ++ " file.") $
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


getJsonErrorInfo :: Json.Error -> String -> (String, Aeson.Value, String)
getJsonErrorInfo err location =
  case err of
    Json.Field field subErr ->
      getJsonErrorInfo subErr $
        if Text.isInfixOf "-" field then
          location ++ "[\"" ++ Text.unpack field ++ "\"]"

        else
          location ++ "." ++ Text.unpack field

    Json.Index index subErr ->
      getJsonErrorInfo subErr $
        location ++ "[" ++ show index ++ "]"

    Json.Failure value expecting ->
      (location, value, expecting)
