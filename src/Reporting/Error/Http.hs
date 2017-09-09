{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Http
  ( Error(..)
  , toReport
  )
  where

import Text.PrettyPrint.ANSI.Leijen ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Json.Decode as Decode
import qualified Reporting.Error.Help as Help
import qualified Reporting.Error.Json as Json



-- ERRORS


data Error
  = Unknown String
  | BadJson (Maybe Decode.Error)
  | BadZipData
  | BadZipSha String String



-- TO REPORT


toReport :: String -> Error -> Help.Report
toReport url err =
  let
    urlDoc =
      P.indent 4 $ P.dullyellow $ "<" <> P.text url <> ">"
  in
  case err of
    Unknown message ->
      Help.report "HTTP PROBLEM" Nothing "The following HTTP request failed:"
        [ urlDoc
        , Help.stack
            [ "Here is the error message I was able to extract:"
            , P.indent 4 $ Help.reflow message
            ]
        ]

    BadJson maybeDecodeError ->
      let
        (intro, details) =
          Json.toDoc Json.Url "json" maybeDecodeError
      in
        Help.report "UNEXPECTED JSON" Nothing intro (urlDoc : details)

    BadZipData ->
      Help.report "CORRUPT ZIP" Nothing "I could not unzip the file downloaded from:"
        [ urlDoc
        , Help.reflow $
            "If it is a transient issue, it should be fixed if you try this\
            \ again. If it seems like an Elm problem, please report it though!"
        ]

    BadZipSha expectedHash actualHash ->
      Help.report "CORRUPT ZIP" Nothing "I got an unexpected zip file from:"
        [ urlDoc
        , Help.reflow $
            "I was expecting the hash of content to be " ++ expectedHash
            ++ ", but it is " ++ actualHash
        , Help.reflow $
            "Most likely the package author may have moved the version\
            \ tag, so report it to them and see if that is the issue."
        ]