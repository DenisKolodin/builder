{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Website
  ( getElmJson
  , getNewPackages
  , getAllPackages
  , download
  )
  where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Trans (liftIO)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import System.FilePath ((</>))

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg
import qualified Json.Decode as Decode

import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task



-- GET PACKAGE INFO


getElmJson :: Name -> Version -> Task.Task LBS.ByteString
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
  fetchJson (Decode.list newPkgDecoder) ("all-packages/since/" ++ show index)


newPkgDecoder :: Decode.Decoder ( Name, Version )
newPkgDecoder =
  do  txt <- Decode.text
      case Text.splitOn "@" txt of
        [key, value] ->
          case (,) <$> Pkg.fromText key <*> Pkg.versionFromText value of
            Right newPkg ->
              Decode.succeed newPkg

            Left _ ->
              Decode.fail "a new package like \"elm-lang/core@6.0.1\""

        _ ->
          Decode.fail "a new package like \"elm-lang/core@6.0.1\""



-- ALL PACKAGES


getAllPackages :: Task.Task (Map.Map Name [Version])
getAllPackages =
  fetchJson allPkgsDecoder "all-packages"


allPkgsDecoder :: Decode.Decoder (Map.Map Name [Version])
allPkgsDecoder =
  let
    depair (key, value) =
      (,) <$> Pkg.fromText key <*> pure value
  in
    do  dict <- Decode.dict (Decode.list Pkg.versionDecoder)
        case traverse depair (HashMap.toList dict) of
          Left _ ->
            Decode.fail "valid package names"

          Right pairs ->
            Decode.succeed (Map.fromList pairs)



-- HELPERS


fetchByteString :: String -> Task.Task LBS.ByteString
fetchByteString path =
  Task.fetch path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        return $ Right $ Client.responseBody response


fetchJson :: Decode.Decoder a -> String -> Task.Task a
fetchJson decoder path =
  Task.fetch path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        case Decode.parse decoder (Client.responseBody response) of
          Right value ->
            return (Right value)

          Left _ ->
            return (Left ("I received corrupt JSON from server. TODO explain"))



-- DOWNLOAD


download :: Name -> Version -> Task.Task ()
download name version =
  do  Task.report (Progress.DownloadPkgStart name version)
      let url = toGithubUrl name version
      archive <-
        Task.fetchFromInternet url $ \request manager ->
          Client.withResponse request manager toArchive
      writeGitHubArchive name version archive
      Task.report (Progress.DownloadPkgEnd name version Progress.Good)


toGithubUrl :: Pkg.Name -> Pkg.Version -> String
toGithubUrl name version =
  "https://github.com/" ++ Pkg.toUrl name ++ "/zipball/" ++ Pkg.versionToString version ++ "/"



-- RESPONSE TO ZIP ARCHIVE


toArchive :: Client.Response Client.BodyReader -> IO (Either String Zip.Archive)
toArchive response =
  toArchiveHelp
    (Binary.runGetIncremental Binary.get)
    (Client.responseBody response)


toArchiveHelp :: Binary.Decoder a -> Client.BodyReader -> IO (Either String a)
toArchiveHelp decoder bodyReader =
  case decoder of
    Binary.Done _ _ value ->
      return $ Right value

    Binary.Fail _ _ _ ->
      return $ Left "seems like the .zip is corrupted"

    Binary.Partial k ->
      do  c <- Client.brRead bodyReader
          let chunk = if BS.null c then Nothing else Just c
          toArchiveHelp (k chunk) bodyReader



-- WRITE GITHUB ARCHIVE


writeGitHubArchive :: Name -> Version -> Zip.Archive -> Task.Task ()
writeGitHubArchive name version archive =
  do  cache <- Task.getPackageCacheDir
      let opts = [Zip.OptDestination (cache </> Pkg.toFilePath name)]
      let vsn = Pkg.versionToString version
      let entries = map (replaceRoot vsn) (Zip.zEntries archive)
      liftIO $ mapM_ (Zip.writeEntry opts) entries


replaceRoot :: String -> Zip.Entry -> Zip.Entry
replaceRoot root entry =
  let
    rootless =
      dropWhile (/='/') (Zip.eRelativePath entry)
  in
    entry { Zip.eRelativePath = root ++ rootless }



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

