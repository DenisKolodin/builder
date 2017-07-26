{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Help
  ( hintLink
  , stack
  , reflow
  , note
  , makeErrorDoc
  , toString
  , toStdout
  , toStderr
  )
  where

import qualified Data.List as List
import GHC.IO.Handle (hIsTerminalDevice)
import System.IO (Handle, hPutStr, stderr, stdout)
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), displayS, displayIO, fillSep, hardline
  , plain, red, renderPretty, text, underline
  )

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg



-- HINT LINKS


hintLink :: String -> String
hintLink hintName =
  "<https://github.com/elm-lang/elm-compiler/blob/"
  ++ Pkg.versionToString Compiler.version
  ++ "/hints/" ++ hintName ++ ".md>"



-- HELPERS


stack :: [Doc] -> Doc
stack allDocs =
  case allDocs of
    [] ->
      error "Do not use `stack` on empty lists."

    doc : docs ->
      List.foldl' verticalAppend doc docs


verticalAppend :: Doc -> Doc -> Doc
verticalAppend a b =
  a <> hardline <> hardline <> b


reflow :: String -> Doc
reflow paragraph =
  fillSep (map text (words paragraph))


note :: String -> Doc
note details =
  fillSep $
    (underline "Note" <> ":") : map text (words details)


makeErrorDoc :: String -> [Doc] -> Doc
makeErrorDoc summary details =
  let
    summaryDoc =
      fillSep (errorStart : map text (words summary))
  in
    stack (summaryDoc : details)
    <> hardline
    <> hardline


errorStart :: Doc
errorStart =
  red (underline "Error") <> ":"



-- OUTPUT


toString :: Doc -> String
toString doc =
  displayS (renderPretty 1 80 (plain doc)) ""


toStdout :: Doc -> IO ()
toStdout doc =
  toHandle stdout doc


toStderr :: Doc -> IO ()
toStderr doc =
  toHandle stderr doc


toHandle :: Handle -> Doc -> IO ()
toHandle handle doc =
  do  isTerminal <- hIsTerminalDevice handle
      if isTerminal
        then displayIO handle (renderPretty 1 80 doc)
        else hPutStr handle (toString doc)
