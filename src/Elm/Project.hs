{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( Project(..)
  , AppInfo(..)
  , PkgInfo(..), Repo(..)
  , Bundles(..)
  , toName, toPkgName, toSourceDir, toNative
  , parse
  , forcePkg
  , path, unsafeRead, write
  )
  where

import Prelude hiding (read)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector as Vector

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Elm.Assets as Assets
import qualified Elm.Project.Constraint as C
import qualified Elm.Project.Licenses as Licenses
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import Reporting.Error.Assets (ProjectError)
import qualified Reporting.Task as Task



-- PROJECT


data Project
  = App AppInfo
  | Pkg PkgInfo


data AppInfo =
  AppInfo
    { _app_elm_version :: Pkg.Version
    , _app_dependencies :: ExactDeps
    , _app_test_deps :: ExactDeps
    , _app_exact_deps :: ExactDeps
    , _app_source_dir :: FilePath
    , _app_cache_dir :: FilePath
    , _app_output_dir :: FilePath
    , _app_bundles :: Bundles
    }


type ExactDeps =
  Map.Map Pkg.Name Pkg.Version


data Bundles = Bundles


data PkgInfo =
  PkgInfo
    { _pkg_repo :: Repo
    , _pkg_summary :: Text
    , _pkg_license :: Licenses.License
    , _pkg_version :: Pkg.Version
    , _pkg_source_dir :: FilePath
    , _pkg_cache_dir :: FilePath
    , _pkg_exposed :: [Module.Raw]
    , _pkg_dependencies :: Constraints
    , _pkg_test_deps :: Constraints
    , _pkg_exact_deps :: ExactDeps
    , _pkg_elm_version :: C.Constraint
    , _pkg_natives :: Bool
    , _pkg_effects :: Bool
    }


type Constraints =
  Map.Map Pkg.Name C.Constraint


destruct :: (AppInfo -> a) -> (PkgInfo -> a) -> Project -> a
destruct appFunc pkgFunc project =
  case project of
    App info ->
      appFunc info

    Pkg info ->
      pkgFunc info



-- REPO


data Repo
  = GitHub Text Text
  | GitLab Text Text
  | BitBucket Text Text


toName :: Project -> Maybe Pkg.Name
toName project =
  destruct (const Nothing) (Just . toPkgName) project


toPkgName :: PkgInfo -> Pkg.Name
toPkgName info =
  case _pkg_repo info of
    GitHub user project ->
      Pkg.Name user project

    GitLab user project ->
      Pkg.Name user project

    BitBucket user project ->
      Pkg.Name user project



-- SOURCE DIRECTORIES


toSourceDir :: Project -> FilePath
toSourceDir project =
  destruct _app_source_dir _pkg_source_dir project



-- NATIVES


toNative :: Project -> Bool
toNative project =
  destruct (const False) _pkg_natives project



-- JSON to PROJECT


parse :: BS.ByteString -> Either ProjectError Project
parse bytestring =
  do  object <- onError E.BadJson (Json.eitherDecode bytestring)
      case HashMap.lookup "type" object of
        Just "application" ->
          App <$> parseAppInfo object

        Just "package" ->
          Pkg <$> parsePkgInfo object

        _ ->
          Left $ E.BadType


parseAppInfo :: Json.Object -> Either ProjectError AppInfo
parseAppInfo obj =
  do  a <- get obj "elm-version" (checkVersion Nothing)
      b <- get obj "dependencies" checkAppDeps
      c <- get obj "test-deps" checkAppDeps
      d <- get obj "exact-deps" checkAppDeps
      e <- get obj "source-directory" checkDir
      f <- get obj "cache-directory" checkDir
      g <- get obj "output-directory" checkDir
      h <- get obj "bundles" checkBundles
      return (AppInfo a b c d e f g h)


parsePkgInfo :: Json.Object -> Either ProjectError PkgInfo
parsePkgInfo obj =
  do  a <- get obj "repo" checkRepo
      b <- get obj "summary" checkSummary
      c <- get obj "license" checkLicense
      d <- get obj "version" (checkVersion Nothing)
      e <- get obj "source-directory" checkDir
      f <- get obj "cache-directory" checkDir
      g <- get obj "exposed-modules" checkExposed
      h <- get obj "dependencies" checkPkgDeps
      i <- get obj "test-deps" checkPkgDeps
      j <- get obj "exact-deps" checkAppDeps
      k <- get obj "elm-version" (checkConstraint Nothing)
      l <- getFlag obj "native-modules"
      m <- getFlag obj "effect-modules"
      return (PkgInfo a b c d e f g h i j k l m)



-- JSON HELPERS


type Field = Text


get :: Json.Object -> Field -> Checker a -> Either ProjectError a
get obj field checker =
  case HashMap.lookup field obj of
    Just value ->
      checker field value

    Nothing ->
      Left $ E.Missing field


getFlag :: Json.Object -> Field -> Either ProjectError Bool
getFlag obj field =
  case HashMap.lookup field obj of
    Nothing ->
      Right False

    Just (Json.Bool bool) ->
      Right bool

    _ ->
      Left $ E.BadFlag field


onError :: y -> Either x a -> Either y a
onError err result =
  mapError (const err) result


mapError :: (x -> y) -> Either x a -> Either y a
mapError func result =
  case result of
    Right a ->
      Right a

    Left err ->
      Left (func err)


jsonToString :: Json.Value -> ProjectError -> Either ProjectError String
jsonToString value err =
  Text.unpack <$> jsonToText value err


jsonToText :: Json.Value -> ProjectError -> Either ProjectError Text
jsonToText value err =
  case value of
    Json.String text ->
      Right text

    _ ->
      Left err


getObject :: Json.Value -> ProjectError -> Either ProjectError Json.Object
getObject value err =
  case value of
    Json.Object object ->
      Right object

    _ ->
      Left err



-- CHECKERS


type Checker a =
  Field -> Json.Value -> Either ProjectError a


checkVersion :: Maybe Text -> Checker Pkg.Version
checkVersion context field value =
  do  let err = E.BadVersion context field
      string <- jsonToString value err
      onError err (Pkg.versionFromString string)


checkConstraint :: Maybe Text -> Checker C.Constraint
checkConstraint context field value =
  do  let err = E.BadConstraint context field
      string <- jsonToString value err
      maybe (Left err) Right (C.fromString string)


checkAppDeps :: Checker ExactDeps
checkAppDeps field value =
  do  hashMap <- getObject value (E.BadAppDeps field)
      let deps = HashMap.toList hashMap
      Map.fromList <$> traverse (checkAppDepsHelp field) deps


checkAppDepsHelp :: Field -> (Text.Text, Json.Value) -> Either ProjectError (Pkg.Name, Pkg.Version)
checkAppDepsHelp field (subField, rawVersion) =
  do  let err = E.BadAppDepName field subField
      name <- onError err (Pkg.fromString (Text.unpack subField))
      version <- checkVersion (Just field) subField rawVersion
      return (name, version)


checkPkgDeps :: Checker Constraints
checkPkgDeps field value =
  do  hashMap <- getObject value (E.BadPkgDeps field)
      let deps = HashMap.toList hashMap
      Map.fromList <$> traverse (checkConstraintsHelp field) deps


checkConstraintsHelp :: Field -> (Text.Text, Json.Value) -> Either ProjectError (Pkg.Name, C.Constraint)
checkConstraintsHelp field (text, rawConstraint) =
  do  let err = E.BadPkgDepName field text
      name <- onError err (Pkg.fromString (Text.unpack text))
      constraint <- checkConstraint (Just field) text rawConstraint
      return (name, constraint)


checkDir :: Checker FilePath
checkDir field value =
  jsonToString value (E.BadDir field)


checkSummary :: Checker Text
checkSummary _field value =
  do  summary <- jsonToText value E.BadSummary
      if Text.length summary < 80
        then Right summary
        else Left E.BadSummary


checkLicense :: Checker Licenses.License
checkLicense _field value =
  do  license <- jsonToText value (E.BadLicense [])
      mapError E.BadLicense (Licenses.check license)


checkRepo :: Checker Repo
checkRepo field value =
  do  let err = E.BadRepo field
      text <- jsonToText value err
      case Text.splitOn "/" text of
        [host, author, project] ->
          case host of
            "github.com" ->
              GitHub author <$> checkProject field project

            "gitlab.com" ->
              GitLab author <$> checkProject field project

            "bitbucket.com" ->
              BitBucket author <$> checkProject field project

            _ ->
              Left err

        _ ->
          Left err


checkProject :: Field -> Text -> Either ProjectError Text
checkProject _field _rawProjectName =
  error "TODO - make sure it is a valid project name / probably share with Elm.Package"


checkBundles :: Checker Bundles
checkBundles _field _value =
  error "TODO"


checkExposed :: Checker [Module.Raw]
checkExposed field value =
  case value of
    Json.Array vector ->
      do  let getName entry = jsonToText entry (E.BadExposed field)
          nameVector <- traverse getName vector
          let names = Vector.toList nameVector
          mapError (E.BadExposedName field) $
            traverse getModuleName names

    _ ->
      Left $ E.BadExposed field


getModuleName :: Text -> Either Text Module.Raw
getModuleName text =
  case Module.nameFromString (Text.unpack text) of
    Just name ->
      Right name

    Nothing ->
      Left text



-- CASTING PROJECTS


forcePkg :: Project -> Task.Task PkgInfo
forcePkg project =
  case project of
    App _ ->
      Task.throw (error "TODO")

    Pkg info ->
      return info



-- READING AND WRITING FILES


path :: FilePath
path =
  Assets.projectPath



-- READ


unsafeRead :: FilePath -> Task.Task Project
unsafeRead filePath =
  do  byteString <- liftIO (BS.readFile filePath)
      case parse byteString of
        Right project ->
          return project

        Left err ->
          Task.throw (Error.Assets (E.CorruptProject path err))



-- WRITE


write :: FilePath -> Project -> IO ()
write filePath project =
  writeFile filePath (projectToString project)


projectToString :: Project -> String
projectToString _project =
  error "TODO"
