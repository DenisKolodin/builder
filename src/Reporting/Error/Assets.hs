{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Assets
  ( Error(..)
  , ElmJsonProblem(..)
  , BuildPlanProblem(..)
  , toDoc
  )
  where

import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Package as Pkg

import qualified Json.Decode as Decode
import qualified Reporting.Error.Help as Help
import qualified Reporting.Error.Json as Json



-- ERRORS


data Error
  = BadElmJson ElmJsonProblem
  | BadBuildPlan BuildPlanProblem
  | CorruptElmJson Pkg.Name Pkg.Version
  | CorruptDocumentation Pkg.Name Pkg.Version
  | CorruptBinary FilePath


data ElmJsonProblem
  = BadJson (Maybe Decode.Error)
  | BadDepDup String String Pkg.Name [Pkg.Name]
  | BadSrcDir FilePath


data BuildPlanProblem
  = BadPlanJson (Maybe Decode.Error)



-- TO DOC


toDoc :: Error -> P.Doc
toDoc err =
  case err of
    BadElmJson problem ->
      elmJsonToDoc problem

    BadBuildPlan problem ->
      buildPlanToDoc problem

    CorruptElmJson pkg vsn ->
      corruptJsonToDoc "elm.json" pkg vsn

    CorruptDocumentation pkg vsn ->
      corruptJsonToDoc "docs.json" pkg vsn

    CorruptBinary path ->
      Help.makeErrorDoc
        ("The binary data at " ++ path ++ " is corrupt.")
        [ Help.reflow $
            "Maybe a program is modifying your elm-stuff/ or ELM_HOME\
            \ directory in unexpected ways? Both of those are just caches, so\
            \ you can try deleting them and they will be rebuilt from scratch."
        ]



-- CORRUPT JSON


corruptJsonToDoc :: FilePath -> Pkg.Name -> Pkg.Version -> P.Doc
corruptJsonToDoc path pkg vsn =
  Help.makeErrorDoc
    ( "The " ++ path ++ " for " ++ Pkg.toString pkg
      ++ " " ++ Pkg.versionToString vsn ++ " got corrupted somehow."
    )
    [ Help.reflow $
        "I removed it from my file cache, so if it was some transient\
        \ error it should be fixed if you try the same thing again.\
        \ Please report this if it seems like an Elm problem though!"
    ]



-- BAD ELM JSON


elmJsonToDoc :: ElmJsonProblem -> P.Doc
elmJsonToDoc problem =
  case problem of
    BadJson maybeDecodeError ->
      makeElmJsonDoc $
        Json.toDoc (Json.Path "elm.json") "project" maybeDecodeError

    BadDepDup field1 field2 dup dups ->
      makeElmJsonDoc $
        badDepDupToDoc field1 field2 dup dups

    BadSrcDir dir ->
      Help.makeErrorDoc
        "The \"source-directories\" in your elm.json lists the following directory:"
        [ P.indent 4 (P.dullyellow (P.text dir))
        , Help.reflow "I cannot find that directory though! Is it missing? Is there a typo?"
        ]


makeElmJsonDoc :: ( String, [P.Doc] ) -> P.Doc
makeElmJsonDoc ( overview, details ) =
  let
    link =
      "More help at <https://github.com/elm-lang/elm-package/blob/master/TODO>"
  in
    Help.makeErrorDoc overview (details ++ [ Help.reflow link ])


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



-- BAD BUILD PLAN


buildPlanToDoc :: BuildPlanProblem -> P.Doc
buildPlanToDoc problem =
  case problem of
    BadPlanJson maybeDecodeError ->
      makeBuildPlanDoc $
        Json.toDoc (Json.Path "elm-build-plan.json") "plan" maybeDecodeError


makeBuildPlanDoc :: ( String, [P.Doc] ) -> P.Doc
makeBuildPlanDoc ( overview, details ) =
  let
    link =
      "More help at <https://github.com/elm-lang/elm-package/blob/master/TODO>"
  in
    Help.makeErrorDoc overview (details ++ [ Help.reflow link ])
