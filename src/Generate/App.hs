{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.App
  ( header
  , footer
  )
  where


import qualified Data.ByteString.Builder as BS
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.RawString.QQ (r)

import qualified Elm.Compiler as Elm
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode
import Json.Encode ((==>))

import qualified Generate.Functions as Functions



-- HEADER


header :: BS.Builder
header =
  "(function(){\n'use strict';" <> Functions.functions



-- FOOTER


footer :: Maybe Module.Interfaces -> [Module.Canonical] -> BS.Builder
footer debug roots =
  "var Elm = {};\n"
  <> mconcat (map (genExport debug) (List.sort roots))
  <> footerEnd



-- EXPORTS


genExport :: Maybe Module.Interfaces -> Module.Canonical -> BS.Builder
genExport debug canonicalName@(Module.Canonical _ name) =
  let
    programFunction =
      BS.byteString $ Text.encodeUtf8 $ Module.canonicalToMain canonicalName

    nameChunks =
      map Text.unpack (Text.splitOn "." name)

    args =
      "(Elm" <> mconcat (map brackets nameChunks)
      <> ")('" <> BS.stringUtf8 (Module.nameToString name)
      <> "')(" <> genDebugInfo debug canonicalName <> ");"
  in
    "if (typeof " <> programFunction <> " !== 'undefined') {"
    <> genObject nameChunks
    <> "\n  " <> programFunction <> args
    <> "\n}"


genObject :: [String] -> BS.Builder
genObject nameChunks =
  let
    define names =
      let
        object =
          "Elm" <> mconcat (map brackets names)
      in
        "\n  " <> object <> " = " <> object <> " || {};"
  in
    mconcat (map define (tail (List.inits nameChunks)))


brackets :: String -> BS.Builder
brackets name =
  "['" <> BS.stringUtf8 name <> "']"



-- DEBUG INFORMATION


genDebugInfo :: Maybe Module.Interfaces -> Module.Canonical -> BS.Builder
genDebugInfo debug canonicalName =
  case debug of
    Nothing ->
      "undefined"

    Just ifaces ->
      let
        program =
          Module.programTypes ifaces canonicalName

        version =
          Pkg.encodeVersion Elm.version
      in
        Encode.encode $ Encode.object $
          [ "versions" ==> Encode.object [ "elm" ==> version ]
          , "types" ==> maybe Encode.null Type.encodeProgram program
          ]



-- REAL FOOTER


footerEnd :: BS.Builder
footerEnd = [r|
if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);
|]
