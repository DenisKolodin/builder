{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Website
  ( getElmJson
  , getNewPackages
  , getAllPackages
  )
  where

import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg

import qualified Reporting.Task as Task



-- GET PACKAGE INFO


getElmJson :: Name -> Version -> Task.Task BS.ByteString
getElmJson name version =
  fetchByteString (packageUrl name version "elm.json")


packageUrl :: Name -> Version -> FilePath -> String
packageUrl name version filePath =
  "packages/"
  ++ Pkg.toUrl name
  ++ "/"
  ++ Pkg.versionToString version
  ++ "/"
  ++ filePath



-- NEW PACKAGES


getNewPackages :: Int -> Task.Task [(Name, Version)]
getNewPackages index =
  map _newPkg <$> fetchJson ("all-packages/since/" ++ show index)


data NewPkg =
  NewPkg { _newPkg :: (Name, Version) }


instance Json.FromJSON NewPkg where
  parseJSON =
    Json.withText "new-package" $ \text ->
      case Text.splitOn "@" text of
        [key, value] ->
          do  name <- Json.parseJSON (Json.String key)
              vsn <- Json.parseJSON (Json.String value)
              return (NewPkg (name, vsn))
        _ ->
          fail (show text ++ " cannot have multiple @ symbols.")



-- ALL PACKAGES


getAllPackages :: Task.Task (Map.Map Name [Version])
getAllPackages =
  _allPkgs <$> fetchJson "all-packages"


data AllPkgs =
  AllPkgs { _allPkgs :: Map.Map Name [Version] }


instance Json.FromJSON AllPkgs where
  parseJSON =
    let
      depair (key, value) =
        do  name <- Json.parseJSON (Json.String key)
            versions <- Json.parseJSON value
            return (name, versions)
    in
      Json.withObject "all-packages" $ \obj ->
        do  kvs <- traverse depair (HashMap.toList obj)
            return (AllPkgs (Map.fromList kvs))



-- HELPERS


fetchBinary :: (Binary.Binary a) => String -> Task.Task a
fetchBinary path =
  Task.fetch path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        let bits = Client.responseBody response
        case Binary.decodeOrFail bits of
          Right (_, _, value) ->
            return (Right value)

          Left (_, _, msg) ->
            return (Left ("I received corrupt binary data from server.\n" ++ msg))


fetchByteString :: String -> Task.Task BS.ByteString
fetchByteString path =
  Task.fetch path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        return $ Right $ Client.responseBody response


fetchJson :: (Json.FromJSON a) => String -> Task.Task a
fetchJson path =
  Task.fetch path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        let bits = Client.responseBody response
        case Json.eitherDecode bits of
          Right value ->
            return (Right value)

          Left msg ->
            return (Left ("I received corrupt JSON from server.\n" ++ msg))



-- REGISTER PACKAGES


register :: Name -> Version -> Task.Task ()
register name version =
  let
    params =
      [ ("name", Pkg.toString name)
      , ("version", Pkg.versionToString version)
      ]

    files =
      [ Multi.partFileSource "documentation" "documentation.json"
      , Multi.partFileSource "description" "description.json"
      , Multi.partFileSource "readme" "README.md"
      ]
  in
    Task.fetch "register" params $ \request manager ->
      do  request' <- Multi.formDataBody files request
          let request'' = request' { Client.responseTimeout = Nothing }
          Client.httpLbs request'' manager
          return (Right ())

