{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Json
  ( Project(..)
  , RawProject(..)
  , AppInfo(..)
  , PkgInfo(..)
  , defaultSummary
  -- json
  , write
  , encode
  , decoder
  , pkgDecoder
  -- checks
  , checkPkgOverlap
  , checkAppOverlap
  -- queries
  , appSolution
  , isKernel
  , isPackageRoot
  , get
  , getName
  , getSourceDirs
  , getEffect
  )
  where

import Data.Text (Text)
import Prelude hiding (read)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Map (Map)
import Data.Set (Set)
import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Licenses as Licenses
import qualified Generate.Plan as Plan
import qualified Json.Decode as D
import qualified Json.Encode as E



-- PROJECT


data Project
  = App AppInfo (Maybe Plan.Plan)
  | Pkg PkgInfo


data RawProject
  = RawApp AppInfo
  | RawPkg PkgInfo



-- APPLICATION


data AppInfo =
  AppInfo
    { _app_elm_version :: Version
    , _app_source_dirs :: [FilePath]
    , _app_deps :: Map Name Version
    , _app_test_deps :: Map Name Version
    , _app_trans_deps :: Map Name Version
    }



-- PACKAGE


data PkgInfo =
  PkgInfo
    { _pkg_name :: Name
    , _pkg_summary :: Text
    , _pkg_license :: Licenses.License
    , _pkg_version :: Version
    , _pkg_exposed :: [Module.Raw]
    , _pkg_deps :: Map Name Con.Constraint
    , _pkg_test_deps :: Map Name Con.Constraint
    , _pkg_elm_version :: Con.Constraint
    , _pkg_effects :: Bool
    }



-- DEFAULTS


defaultSummary :: Text
defaultSummary =
  "helpful summary of your project, less than 80 characters"



-- CHECKS


checkPkgOverlap :: PkgInfo -> Maybe (Set Name)
checkPkgOverlap (PkgInfo _ _ _ _ _ deps tests _ _) =
  filterEmpties $
    checkOverlap deps tests


checkAppOverlap :: AppInfo -> Maybe (Set Name)
checkAppOverlap (AppInfo _ _ deps tests trans) =
  filterEmpties $
    Set.unions
      [ checkOverlap deps tests
      , checkOverlap deps trans
      , checkOverlap tests trans
      ]


checkOverlap :: Map Name a -> Map Name a -> Set Name
checkOverlap deps1 deps2 =
  Map.keysSet $ Map.intersection deps1 deps2


filterEmpties :: Set a -> Maybe (Set a)
filterEmpties set =
  if Set.null set then Nothing else Just set



-- QUERIES


appSolution :: AppInfo -> Map Name Version
appSolution info =
  Map.unions
    [ _app_deps info
    , _app_test_deps info
    , _app_trans_deps info
    ]


isKernel :: Project -> Bool
isKernel project =
  case project of
    App _ _ ->
      False

    Pkg info ->
      let
        (Pkg.Name user _) =
          _pkg_name info
      in
        user == "elm-lang" || user == "elm-exploration"


isPackageRoot :: Module.Raw -> Project -> Bool
isPackageRoot name project =
  case project of
    App _ _ ->
      False

    Pkg info ->
      elem name (_pkg_exposed info)


get :: (AppInfo -> a) -> (PkgInfo -> a) -> Project -> a
get appFunc pkgFunc project =
  case project of
    App info _ ->
      appFunc info

    Pkg info ->
      pkgFunc info


getName :: Project -> Name
getName project =
  get (\_ -> Pkg.dummyName) _pkg_name project


getSourceDirs :: Project -> [FilePath]
getSourceDirs project =
  get _app_source_dirs (\_ -> ["src"]) project


getEffect :: Project -> Bool
getEffect project =
  get (const False) _pkg_effects project



-- WRITE


write :: FilePath -> Project -> IO ()
write root project =
  E.write (root </> "elm.json") (encode project)



-- JSON ENCODE


encode :: Project -> E.Value
encode project =
  case project of
    App (AppInfo elm srcDirs deps tests trans) _ ->
      E.object
        [ "type" ==> E.string "application"
        , "source-directories" ==> E.list E.string srcDirs
        , "elm-version" ==> encodeVersion elm
        , "dependencies" ==> encodeDeps encodeVersion deps
        , "test-dependencies" ==> encodeDeps encodeVersion tests
        , "do-not-edit-this-by-hand" ==>
            E.object [ "transitive-dependencies" ==> encodeDeps encodeVersion trans ]
        ]

    Pkg (PkgInfo name summary license version exposed deps tests elm effects) ->
      E.object $
        [ "type" ==> E.string "package"
        , "name" ==> E.string (Pkg.toString name)
        , "summary" ==> E.text summary
        , "license" ==> Licenses.encode license
        , "version" ==> E.text (Pkg.versionToText version)
        , "exposed-modules" ==> E.list E.text exposed
        , "elm-version" ==> encodeConstraint elm
        , "dependencies" ==> encodeDeps encodeConstraint deps
        , "test-dependencies" ==> encodeDeps encodeConstraint tests
        ]
        ++ if effects then [ "effect-modules" ==> E.bool True ] else []


(==>) :: a -> b -> (a, b)
(==>) a b =
  (a, b)


encodeDeps :: (a -> E.Value) -> Map Pkg.Name a -> E.Value
encodeDeps encodeValue deps =
  E.dict Pkg.toString encodeValue deps


encodeConstraint :: Con.Constraint -> E.Value
encodeConstraint constraint =
  E.text (Con.toText constraint)


encodeVersion :: Pkg.Version -> E.Value
encodeVersion version =
  E.text (Pkg.versionToText version)



-- JSON DECODE


decoder :: D.Decoder RawProject
decoder =
  do  tipe <- D.field "type" D.text
      case tipe of
        "application" ->
          D.map RawApp appDecoder

        "package" ->
          D.map RawPkg pkgDecoder

        _ ->
          D.fail "\"application\" or \"package\""


appDecoder :: D.Decoder AppInfo
appDecoder =
  AppInfo
    <$> D.field "elm-version" versionDecoder
    <*> D.field "source-directories" (D.list dirDecoder)
    <*> D.field "dependencies" (depsDecoder versionDecoder)
    <*> D.field "test-dependencies" (depsDecoder versionDecoder)
    <*> D.at ["do-not-edit-this-by-hand", "transitive-dependencies"] (depsDecoder versionDecoder)


pkgDecoder :: D.Decoder PkgInfo
pkgDecoder =
  PkgInfo
    <$> D.field "name" Pkg.decoder
    <*> D.field "summary" summaryDecoder
    <*> D.field "license" licenseDecoder
    <*> D.field "version" versionDecoder
    <*> D.field "exposed-modules" (D.list Module.decoder)
    <*> D.field "dependencies" (depsDecoder constraintDecoder)
    <*> D.field "test-dependencies" (depsDecoder constraintDecoder)
    <*> D.field "elm-version" constraintDecoder
    <*> flag "effect-modules"


flag :: Text -> D.Decoder Bool
flag name =
  maybe False id <$>
    D.maybe (D.field name D.bool)



-- JSON DECODE HELPERS


versionDecoder :: D.Decoder Version
versionDecoder =
  do  txt <- D.text
      either fail return (Pkg.versionFromText txt)


constraintDecoder :: D.Decoder Con.Constraint
constraintDecoder =
  do  txt <- D.text
      case Con.fromText txt of
        Just constraint ->
          D.succeed constraint

        Nothing ->
          D.fail "a valid constraint, like \"1.0.0 <= v < 2.0.0\""


depsDecoder :: D.Decoder a -> D.Decoder (Map Name a)
depsDecoder valueDecoder =
  do  hashMap <- D.dict valueDecoder
      keyValues <- traverse validateKey (HashMap.toList hashMap)
      return (Map.fromList keyValues)


validateKey :: (Text, a) -> D.Decoder (Name, a)
validateKey (key, value) =
  case Pkg.fromText key of
    Left _ ->
      D.fail "keys that are valid project names"

    Right name ->
      D.succeed (name, value)


dirDecoder :: D.Decoder FilePath
dirDecoder =
  do  maybeText <- D.maybe D.text
      case maybeText of
        Nothing ->
          D.fail "a file path"

        Just txt ->
          D.succeed (Text.unpack txt)


summaryDecoder :: D.Decoder Text
summaryDecoder =
  do  summary <- D.text
      if Text.length summary < 80
        then D.succeed summary
        else D.fail "a summary less than 80 characters long"


licenseDecoder :: D.Decoder Licenses.License
licenseDecoder =
  do  txt <- D.text
      case Licenses.check txt of
        Left suggestions ->
          error "TODO license" suggestions

        Right license ->
          D.succeed license



