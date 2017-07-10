{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Assets
  ( Error(..)
  , BadJson(..)
  , ElmJsonProblem(..)
  , BuildPlanProblem(..)
  , toDoc
  )
  where

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

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified Reporting.Error.Help as Help
import Reporting.Error.Help (reflow)



-- ERRORS


data Error
  = BadElmJson (BadJson ElmJsonProblem)
  | BadBuildPlan (BadJson BuildPlanProblem)
  | CorruptElmJson Pkg.Name Pkg.Version
  | CorruptDocumentation Pkg.Name Pkg.Version
  | CorruptVersionCache Pkg.Name
  | PackageNotFound Pkg.Name [Pkg.Name]
  | CorruptBinary FilePath


data BadJson problem
  = BadSyntax
  | BadStructure Decode.Error
  | BadContent problem


data ElmJsonProblem
  = BadDepDup String String Pkg.Name [Pkg.Name]
  | BadSrcDir FilePath


data BuildPlanProblem



-- TO DOC


toDoc :: Error -> P.Doc
toDoc err =
  case err of
    BadElmJson badJson ->
      case badJson of
        BadSyntax ->
          makeElmJsonDoc $ badSyntax "elm.json"

        BadStructure decodeError ->
          makeElmJsonDoc $ decodeErrorToDoc "elm.json" "project" decodeError

        BadContent (BadDepDup field1 field2 dup dups) ->
          makeElmJsonDoc $ badDepDupToDoc field1 field2 dup dups

        BadContent (BadSrcDir dir) ->
          Help.makeErrorDoc
            "The \"source-directories\" in your elm.json lists the following directory:"
            [ P.indent 4 (P.dullyellow (P.text dir))
            , Help.reflow "I cannot find that directory though! Is it missing? Is there a typo?"
            ]

    BadBuildPlan badJson ->
      case badJson of
        BadSyntax ->
          makeBuildPlanDoc $ badSyntax "elm-build-plan.json"

        BadStructure decodeError ->
          makeBuildPlanDoc $ decodeErrorToDoc "elm-build-plan.json" "plan" decodeError

        BadContent _ ->
          error "TODO handle bad content for build plans"

    CorruptElmJson pkg vsn ->
      error $ "TODO CorruptElmJson " ++ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn

    CorruptDocumentation name version ->
      Help.makeErrorDoc "CorruptDocumentation" [ P.text "TODO" ]

    CorruptVersionCache name ->
      Help.makeErrorDoc "CorruptVersionCache" [ P.text "TODO" ]

    PackageNotFound package suggestions ->
      Help.makeErrorDoc
        ( "Could not find any packages named " ++ Pkg.toString package
        )
        [ P.text $ "Maybe you want one of these instead?"
        , P.indent 4 $ P.vcat $ map (P.text . Pkg.toString) suggestions
        ]

    CorruptBinary path ->
      Help.makeErrorDoc "CorruptBinary" [ P.text $ "TODO " ++ path ]



-- MAKE DOCS WITH LINKS


makeElmJsonDoc :: ( String, [P.Doc] ) -> P.Doc
makeElmJsonDoc ( overview, details ) =
  let
    link =
      "More help at <https://github.com/elm-lang/elm-package/blob/master/TODO>"
  in
    Help.makeErrorDoc overview (details ++ [ Help.reflow link ])


makeBuildPlanDoc :: ( String, [P.Doc] ) -> P.Doc
makeBuildPlanDoc ( overview, details ) =
  let
    link =
      "More help at <https://github.com/elm-lang/elm-package/blob/master/TODO>"
  in
    Help.makeErrorDoc overview (details ++ [ Help.reflow link ])



-- JSON - BAD SYNTAX


badSyntax :: FilePath -> ( String, [P.Doc] )
badSyntax path =
  ( "The content of " ++ path ++ " is not valid JSON."
  , [ reflow "Maybe a comma is missing? Or a closing } or ]?"
    ]
  )



-- JSON - BAD STRUCTURE


decodeErrorToDoc :: FilePath -> String -> Decode.Error -> ( String, [P.Doc] )
decodeErrorToDoc path rootName err =
  case flatten err of
    [] ->
      ( "I am struggling with the JSON in " ++ path
      , [ Help.reflow $
            "I have no idea what is wrong though, which is weird. Please report an\
            \ issue to <https://github.com/evancz/builder/issues> with your entire "
            ++ path ++ " file so we can try to do better!"
        ]
      )

    [flatError] ->
      ( "I am struggling with the JSON in " ++ path
      , [ flatErrorToDoc rootName [] flatError
        ]
      )

    flatErrors ->
      let
        toNumberedDoc index flatErr =
          P.dullcyan ("(" <> P.int index <> ")") <+> flatErrorToDoc rootName [] flatErr
      in
        ( "The JSON in " ++ path ++ " is giving me troubles. I have a few theories ("
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



-- JSON - BAD CONTENT


badDepDupToDoc :: String -> String -> Pkg.Name -> [Pkg.Name] -> ( String, [P.Doc] )
badDepDupToDoc field1 field2 dup dups =
  let
    introduction =
      if null dups then
        "The following package appears twice in your elm.json file:"
      else
        "The following packages appear twice in your elm.json file:"

    packagesAre =
      if null dups then "package is" else "packages are"

    advice =
      if null dups then
        "Delete one of the entries."
      else
        "Delete duplicates until each package lives in ONE category."
  in
    ( introduction
    , [ P.indent 4 $ P.vcat $
          map (P.dullyellow . P.text . Pkg.toString) (dup:dups)
      , Help.reflow $
          "The " ++ packagesAre ++ " available in \"" ++ field1
          ++ "\", so it is redundant to list it again in \"" ++ field2
          ++ "\". It is already available! " ++ advice
      ]
    )
