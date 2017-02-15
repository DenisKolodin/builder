{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Internal
  ( Project(..)
  , AppInfo(..)
  , PkgInfo(..)
  , TransitiveDeps(..)
  , Bundles(..)
  , parse
  )
  where

import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (read)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Map (Map)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import qualified Elm.Project.Constraint as C
import qualified Elm.Project.Licenses as Licenses
import qualified Utils.Json as Json



-- PROJECT


data Project
  = App AppInfo
  | Pkg PkgInfo
  deriving (Eq, Generic)



-- APP INFO


data AppInfo =
  AppInfo
    { _app_elm_version :: Version
    , _app_pages :: [Module.Raw]
    , _app_bundles :: Bundles
    , _app_source_dir :: FilePath
    , _app_output_dir :: FilePath
    , _app_deps :: TransitiveDeps
    }
    deriving (Eq, Generic)


data TransitiveDeps =
  TransitiveDeps
    { _direct :: Map Name Version
    , _trans :: Map Name Version
    , _test_direct :: Map Name Version
    , _test_trans :: Map Name Version
    }
    deriving (Eq, Generic)


data Bundles = Bundles [[Name]]
  deriving (Eq, Generic)



-- PKG INFO


data PkgInfo =
  PkgInfo
    { _pkg_name :: Name
    , _pkg_summary :: Text
    , _pkg_license :: Licenses.License
    , _pkg_version :: Version
    , _pkg_exposed :: [Module.Raw]
    , _pkg_dependencies :: Constraints
    , _pkg_test_deps :: Constraints
    , _pkg_transitive_deps :: TransitiveDeps
    , _pkg_elm_version :: C.Constraint
    , _pkg_natives :: Bool
    , _pkg_effects :: Bool
    }
    deriving (Eq, Generic)


type Constraints =
  Map Name C.Constraint



-- BINARY


instance Binary Project
instance Binary AppInfo
instance Binary PkgInfo
instance Binary Bundles
instance Binary TransitiveDeps



-- JSON


parse :: BS.ByteString -> Either (Maybe Json.Error) Project
parse bytestring =
  case Aeson.eitherDecode bytestring of
    Left _ ->
      Left Nothing

    Right value ->
      case Json.decode projectDecoder value of
        Left err ->
          Left (Just err)

        Right project ->
          Right project



-- PROJECT JSON


projectDecoder :: Json.Decoder Project
projectDecoder =
  do  tipe <- Json.field "type" Json.text
      case tipe of
        "application" ->
          Json.map App appDecoder

        "package" ->
          Json.map Pkg pkgDecoder

        _ ->
          Json.fail "\"application\" or \"package\""



-- APP JSON


appDecoder :: Json.Decoder AppInfo
appDecoder =
  AppInfo
    <$> Json.field "elm-version" versionDecoder
    <*> Json.field "pages" pagesDecoder
    <*> Json.field "bundles" bundlesDecoder
    <*> Json.field "source-directory" dirDecoder
    <*> Json.field "output-directory" dirDecoder
    <*> appDepsDecoder


appDepsDecoder :: Json.Decoder TransitiveDeps
appDepsDecoder =
  TransitiveDeps
    <$> Json.field "dependencies" (depsDecoder versionDecoder)
    <*> Json.field "test-dependencies" (depsDecoder versionDecoder)
    <*> Json.at ["do-not-edit-this-by-hand", "transitive-dependencies"] (depsDecoder versionDecoder)
    <*> Json.at ["do-not-edit-this-by-hand", "transitive-test-dependencies"] (depsDecoder versionDecoder)



-- PACKAGE JSON


pkgDecoder :: Json.Decoder PkgInfo
pkgDecoder =
  PkgInfo
    <$> Json.field "name" pkgNameDecoder
    <*> Json.field "summary" summaryDecoder
    <*> Json.field "license" licenseDecoder
    <*> Json.field "version" versionDecoder
    <*> Json.field "exposed-modules" exposedDecoder
    <*> Json.field "dependencies" (depsDecoder constraintDecoder)
    <*> Json.field "test-dependencies" (depsDecoder constraintDecoder)
    <*> Json.field "do-not-edit-this-by-hand" pkgTransitiveDepsDecoder
    <*> Json.field "elm-version" constraintDecoder
    <*> flag "native-modules"
    <*> flag "effect-modules"


flag :: Text -> Json.Decoder Bool
flag name =
  maybe False id <$>
    Json.maybe (Json.field name Json.bool)


pkgTransitiveDepsDecoder :: Json.Decoder TransitiveDeps
pkgTransitiveDepsDecoder =
  TransitiveDeps
    <$> Json.field "dependencies" (depsDecoder versionDecoder)
    <*> Json.field "test-dependencies" (depsDecoder versionDecoder)
    <*> Json.field "transitive-dependencies" (depsDecoder versionDecoder)
    <*> Json.field "transitive-test-dependencies" (depsDecoder versionDecoder)



-- JSON HELPERS


versionDecoder :: Json.Decoder Version
versionDecoder =
  do  txt <- Json.text
      either fail return (Pkg.versionFromText txt)


constraintDecoder :: Json.Decoder C.Constraint
constraintDecoder =
  do  txt <- Json.text
      case C.fromText txt of
        Just constraint ->
          Json.succeed constraint

        Nothing ->
          Json.fail "a valid constraint, like \"1.0.0 <= v < 2.0.0\""


depsDecoder :: Json.Decoder a -> Json.Decoder (Map Name a)
depsDecoder decoder =
  do  hashMap <- Json.dict decoder
      keyValues <- traverse validateKey (HashMap.toList hashMap)
      return (Map.fromList keyValues)


validateKey :: (Text, a) -> Json.Decoder (Name, a)
validateKey (key, value) =
  case Pkg.fromText key of
    Left _ ->
      Json.fail "keys that are valid project names"

    Right name ->
      Json.succeed (name, value)


dirDecoder :: Json.Decoder FilePath
dirDecoder =
  do  maybeText <- Json.maybe Json.text
      case maybeText of
        Nothing ->
          Json.fail "a file path"

        Just txt ->
          Json.succeed (Text.unpack txt)


pagesDecoder :: Json.Decoder [Module.Raw]
pagesDecoder =
  Json.list moduleNameDecoder


summaryDecoder :: Json.Decoder Text
summaryDecoder =
  do  summary <- Json.text
      if Text.length summary < 80
        then Json.succeed summary
        else Json.fail "a summary less than 80 characters long"


licenseDecoder :: Json.Decoder Licenses.License
licenseDecoder =
  do  txt <- Json.text
      case Licenses.check txt of
        Left suggestions ->
          error "TODO license" suggestions

        Right license ->
          Json.succeed license


pkgNameDecoder :: Json.Decoder Name
pkgNameDecoder =
  do  txt <- Json.text
      case Pkg.fromText txt of
        Left _ ->
          Json.fail "a valid project name"

        Right name ->
          Json.succeed name


bundlesDecoder :: Json.Decoder Bundles
bundlesDecoder =
  Bundles <$> Json.list (Json.list pkgNameDecoder)


exposedDecoder :: Json.Decoder [Module.Raw]
exposedDecoder =
  Json.list moduleNameDecoder


moduleNameDecoder :: Json.Decoder Module.Raw
moduleNameDecoder =
  do  txt <- Json.text
      case Module.nameFromText txt of
        Nothing ->
          Json.fail "a module name like \"Json.Decode\""

        Just name ->
          Json.succeed name
