{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Commands
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
  -- diff
  = DiffApplication
  | DiffUnknownPackage Pkg.Name [Pkg.Name]
  | DiffUnknownVersion Pkg.Name Pkg.Version [Pkg.Version]



-- TO DOC


toDoc :: Error -> P.Doc
toDoc err =
  case err of
    DiffApplication ->
      Help.makeErrorDoc "I cannot perform diffs on applications, only packages!"
        [ Help.reflow $
            "If you just curious to see a diff, try running this command:"
        , P.indent 4 $ P.green $ "elm diff elm-lang/html 5.1.1 6.0.0"
        ]

    DiffUnknownPackage pkg suggestions ->
      Help.makeErrorDoc
        ( "You are trying to diff against this package:"
        )
        [ P.indent 4 $ P.red $ P.text $ Pkg.toString pkg
        , Help.stack $
            case suggestions of
              [] ->
                [ "I cannot find that package though! Maybe there is a typo?"
                ]

              [suggestion] ->
                [ "I cannot find that package though! Maybe you want this one instead?"
                , P.indent 4 $ P.dullyellow $ P.text (Pkg.toString suggestion)
                ]

              _ ->
                [ "I cannot find that package though! Maybe you want one of these instead?"
                , P.indent 4 $ P.dullyellow $ P.vcat $ map (P.text . Pkg.toString) suggestions
                ]
        ]

    DiffUnknownVersion pkg vsn realVersions ->
      Help.makeErrorDoc
        ( "Version " ++ Pkg.versionToString vsn
          ++ " has never been published, so I cannot diff against it."
        )
        [ "Here are all the versions that HAVE been published:"
        , P.indent 4 $ P.dullyellow $ P.vcat $
            let
              sameMajor v1 v2 = Pkg._major v1 == Pkg._major v2
              mkRow vsns = P.hsep $ map (P.text . Pkg.versionToString) vsns
            in
              map mkRow $ List.groupBy sameMajor (List.sort realVersions)
        , "Want one of those instead?"
        ]
