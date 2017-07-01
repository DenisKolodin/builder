{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Help
  ( stack
  , reflow
  , makeErrorDoc
  , toString
  , toStdout
  , toStderr
  , nearbyNames
  )
  where

import Data.Function (on)
import qualified Data.List as List
import GHC.IO.Handle (hIsTerminalDevice)
import System.IO (Handle, hPutStr, stderr, stdout)
import qualified Text.EditDistance as Dist
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), displayS, displayIO, fillSep, hardline
  , plain, red, renderPretty, text, underline
  )

import qualified Elm.Package as Pkg



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
  red (underline (text "Error")) <> text ":"



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



-- NEARBY NAMES


nearbyNames :: Pkg.Name -> [Pkg.Name] -> [Pkg.Name]
nearbyNames package allPackages =
  let
    name =
      Pkg.toString package

    ratedNames =
      map (\pkg -> (distance name (Pkg.toString pkg), pkg)) allPackages

    sortedNames =
      List.sortBy (compare `on` fst) ratedNames
  in
    map snd $ take 4 sortedNames


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y
