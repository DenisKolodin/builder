{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Json (Location(..), toDoc) where

import Control.Arrow ((***))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified Reporting.Error.Help as Help



-- LOCATION


data Location = Url | Path FilePath


locationToString :: Location -> String
locationToString location =
  case location of
    Url ->
      "at this URL"

    Path path ->
      "in " ++ path



-- TO DOC


toDoc :: Location -> String -> Maybe Decode.Error -> ( String, [P.Doc] )
toDoc location rootName maybeError =
  let overThere = locationToString location in
  case maybeError of
    Nothing ->
      ( "The content " ++ overThere ++ " is not valid JSON."
      , [ Help.reflow "Maybe a comma is missing? Or a closing } or ]?"
        ]
      )

    Just err ->
      case flatten err of
        [] ->
          ( "I am struggling with the JSON " ++ overThere
          , [ Help.reflow $
                "I have no idea what is wrong though, which is weird. Please report an\
                \ issue to <https://github.com/evancz/builder/issues> with the entire\
                \ JSON file so we can try to do better!"
            ]
          )

        [flatError] ->
          ( "I am struggling with the JSON " ++ overThere
          , [ flatErrorToDoc rootName [] flatError
            ]
          )

        flatErrors ->
          let
            toNumberedDoc index flatErr =
              P.dullcyan ("(" <> P.int index <> ")") <+> flatErrorToDoc rootName [] flatErr
          in
            ( "The JSON " ++ overThere ++ " is giving me troubles. I have a few theories ("
              ++ show (length flatErrors) ++ ") on what is going wrong:"
            , zipWith toNumberedDoc [1..] flatErrors
            )


flatErrorToDoc :: String -> [P.Doc] -> FlatError -> P.Doc
flatErrorToDoc rootName revNames flatError =
  case flatError of
    At name subFlat ->
      flatErrorToDoc rootName (P.text name : revNames) subFlat

    BadOneOf ->
      Help.reflow oneOfMessage

    Fail aeson problem problems ->
      let
        problemsDoc =
          case problems of
            [] ->
              reflowProblem problem

            _ ->
              Help.stack
                [ Help.reflow $ "There are " ++ show (1 + length problems) ++ " possible issues:"
                , P.vcat $ map (\p -> "  - " <> reflowProblem p) (problem:problems)
                ]
      in
        Help.stack $
          case reverse revNames of
            [] ->
              [ problemsDoc
              ]

            names ->
              [ P.fillSep $
                  map P.text (words "I ran into trouble with the value at")
                  ++ [ P.dullyellow (P.text rootName <> mconcat names) <> ":" ]
              , P.indent 4 (prettyAeson aeson)
              , problemsDoc
              ]


reflowProblem :: String -> P.Doc
reflowProblem problem =
  let
    wordToDoc word =
      if length word > 1 && all Char.isUpper word then
        P.green (P.text word)
      else
        P.text word
  in
    P.fillSep (map wordToDoc (words problem))



-- PRETTY JSON


prettyAeson :: Aeson.Value -> P.Doc
prettyAeson aeson =
  P.text $ Text.unpack $ Text.decodeUtf8 $
    LBS.toStrict $ Builder.toLazyByteString $
      Encode.encode $ encodeAeson aeson


encodeAeson :: Aeson.Value -> Encode.Value
encodeAeson value =
  case value of
    Aeson.Object hashMap ->
      Encode.object $ map (Text.unpack *** encodeAeson) (HashMap.toList hashMap)

    Aeson.Array vector ->
      Encode.array $ map encodeAeson (Vector.toList vector)

    Aeson.String text ->
      Encode.text text

    Aeson.Number scientific ->
      Encode.number scientific

    Aeson.Bool bool ->
      Encode.bool bool

    Aeson.Null ->
      Encode.null



-- CRUSH and FLATTEN errors


data FlatError
  = At String FlatError
  | BadOneOf
  | Fail Aeson.Value String [String]


flatten :: Decode.Error -> [FlatError]
flatten err =
  List.sortOn (depth 0) $
    flattenHelp (add err empty)


flattenHelp :: CrushedError -> [FlatError]
flattenHelp (CrushedError badOneOf failures subErrors) =
  let
    toFlats (name, crushedError) =
      At name <$> flattenHelp crushedError

    rootFailure =
      case failures of
        [] ->
          BadOneOf

        (json, problem) : rest ->
          Fail json problem $
            map snd rest ++ if badOneOf then [oneOfMessage] else []
  in
    rootFailure : concatMap toFlats (Map.toList subErrors)


oneOfMessage :: String
oneOfMessage =
  "Ran into (Json.Decode.oneOf []) with no possiblities. This always fails!"


depth :: Int -> FlatError -> Int
depth !n flat =
  case flat of
    At _ sub ->
      depth (n+1) sub

    BadOneOf ->
      n

    Fail _ _ _ ->
      n


data CrushedError =
  CrushedError
    { _badOneOf :: Bool
    , _failures :: [ ( Aeson.Value, String ) ]
    , _subErrors :: Map.Map String CrushedError
    }


empty :: CrushedError
empty =
  CrushedError False [] Map.empty


add :: Decode.Error -> CrushedError -> CrushedError
add err crushed@(CrushedError badOneOf failures subErrors) =
  case err of
    Decode.Field field subErr ->
      CrushedError badOneOf failures (addName (fieldToName field) subErr subErrors)

    Decode.Index index subErr ->
      CrushedError badOneOf failures (addName (indexToName index) subErr subErrors)

    Decode.OneOf errors ->
      case errors of
        [] ->
          CrushedError True failures subErrors

        _ ->
          foldr add crushed errors

    Decode.Failure aeson problem ->
      CrushedError badOneOf ((aeson, problem) : failures) subErrors


addName :: String -> Decode.Error -> Map.Map String CrushedError -> Map.Map String CrushedError
addName name err subErrors =
  let
    subAdd maybeCrush =
      Just (add err (maybe empty id maybeCrush))
  in
    Map.alter subAdd name subErrors


fieldToName :: Text.Text -> String
fieldToName field =
  case Text.unpack field of
    [] ->
      "['']"

    string@(char : rest) ->
      if Char.isAlpha char && all Char.isAlphaNum rest then
        '.' : string
      else
        "['" ++ string ++ "']"


indexToName :: Int -> String
indexToName index =
  "[" ++ show index ++ "]"
