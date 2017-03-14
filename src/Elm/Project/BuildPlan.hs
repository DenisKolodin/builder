{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.BuildPlan
  ( BuildPlan(..)
  , Page(..)
  , parse
  )
  where

import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Utils.Json as Json



-- BUILD PLAN


data BuildPlan =
  BuildPlan
    { _cache :: Module.Raw
    , _pages :: [Page]
    , _bundles :: [[Pkg.Name]]
    , _endpoint :: Text
    , _output_dir :: FilePath
    }


data Page =
  Page
    { _elm :: Module.Raw
    , _css :: [Text]
    , _js :: [Text]
    }



-- JSON


parse :: BS.ByteString -> Either (Maybe Json.Error) BuildPlan
parse bytestring =
  Json.parse planDecoder bytestring


planDecoder :: Json.Decoder BuildPlan
planDecoder =
  BuildPlan
    <$> Json.field "cache" Json.moduleName
    <*> Json.field "pages" (Json.list pageDecoder)
    <*> Json.field "bundles" (Json.list (Json.list Json.packageName))
    <*> Json.field "endpoint" Json.text
    <*> Json.field "output-directory" (Json.map Text.unpack Json.text)


pageDecoder :: Json.Decoder Page
pageDecoder =
  Page
    <$> Json.field "elm" Json.moduleName
    <*> Json.field "css" (Json.list Json.text)
    <*> Json.field "js" (Json.list Json.text)

