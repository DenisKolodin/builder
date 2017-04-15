{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Repl
  ( header
  , Output
  , toOutput
  , toRoots
  , footer
  )
  where


import qualified Data.ByteString.Builder as BS
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Monoid ((<>))
import qualified Data.Text as Text

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Compiler.Type as Type
import qualified Elm.Package as Pkg

import qualified Generate.Functions as Functions



-- HEADER


header :: BS.Builder
header =
  "process.on('uncaughtException', function(err) {\n\
  \  process.stderr.write(err.toString());\n\
  \  process.exit(1);\n\
  \});\n\
  \(function(){\n'use strict';" <> Functions.functions



-- OUTPUT


data Output =
  Output
    { _name :: String
    , _type :: Type.Type
    }


toOutput :: Map.Map Module.Raw Compiler.Result -> String -> Output
toOutput results name =
  let
    types =
      Module.interfaceAliasedTypes (Compiler._iface (results ! "ElmRepl"))
  in
    Output name (types ! Text.pack name)


toRoots :: Pkg.Name -> Output -> Obj.Roots
toRoots pkg (Output name _) =
  Obj.value (Module.Canonical pkg "ElmRepl") (name ++ "_as_string_for_repl")



-- FOOTER


footer :: Pkg.Name -> Output -> BS.Builder
footer (Pkg.Name user project) (Output name tipe) =
  let
    jsName =
      utf8 user <> "$" <> utf8 project <> "$ElmRepl$" <> BS.stringUtf8 name

    typeString =
      BS.stringUtf8 (show (Type.toString Type.MultiLine tipe))
  in
    "var _value = " <> jsName <> "_as_string_for_repl;\n" <>
    "var _type = " <> typeString <> ";\n" <>
    "if (_value.length + 3 + _type.length >= 80 || _type.indexOf('\\n') >= 0) {\n\
    \    console.log(_value + '\\n    : ' + _type.split('\\n').join('\\n      '));\n\
    \} else {\n\
    \    console.log(_value + ' : ' + _type);\n\
    \}\n\
    \}());"


utf8 :: Text.Text -> BS.Builder
utf8 text =
  BS.stringUtf8 (Text.unpack (Text.replace "-" "_" text))
