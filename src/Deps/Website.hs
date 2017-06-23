{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Website
  ( getElmJson
  , getNewPackages
  , getAllPackages
  , download
  , register
  )
  where

import Prelude hiding (zip)
import qualified Codec.Archive.Zip as Zip
import Control.Monad (void)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
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
import qualified Reporting.Task.Http as Http



-- GET PACKAGE INFO


getElmJson :: Name -> Version -> Task.Task LBS.ByteString
getElmJson name version =
  Http.run $ fetchByteString (packageUrl name version "elm.json")


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
  Http.run $ fetchJson (Decode.list newPkgDecoder) ("all-packages/since/" ++ show index)


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
  Http.run $ fetchJson allPkgsDecoder "all-packages"


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


fetchByteString :: String -> Http.Fetch LBS.ByteString
fetchByteString path =
  Http.package path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        return $ Right $ Client.responseBody response


fetchJson :: Decode.Decoder a -> String -> Http.Fetch a
fetchJson decoder path =
  Http.package path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        case Decode.parse decoder (Client.responseBody response) of
          Right value ->
            return $ Right value

          Left _ ->
            return $ Left "I received corrupt JSON from server. TODO explain"



-- DOWNLOAD


download :: [(Name, Version)] -> Task.Task ()
download packages =
  case packages of
    [] ->
      Task.report Progress.DownloadSkip

    _ ->
      do  cache <- Task.getPackageCacheDir

          let start = Progress.DownloadStart packages
          let toEnd = Progress.DownloadEnd

          void $ Http.run $ Http.report start toEnd $
            Http.parallel $ map (downloadHelp cache) packages


downloadHelp :: FilePath -> (Name, Version) -> Http.Fetch ()
downloadHelp cache (name, version) =
  let
    endpointUrl =
      "endpoint/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version
  in
    Http.andThen (fetchJson endpointDecoder endpointUrl) $ \(endpoint, hash) ->
      let
        start = Progress.DownloadPkgStart name version
        toEnd = Progress.DownloadPkgEnd name version
      in
        Http.report start toEnd $
          Http.anything endpoint $ \request manager ->
            Client.withResponse request manager (writeArchive cache name version hash)


endpointDecoder :: Decode.Decoder (String, String)
endpointDecoder =
  Decode.map2 (,)
    (Decode.field "url" Decode.string)
    (Decode.field "hash" Decode.string)



-- WRITE ZIP ARCHIVE


writeArchive :: FilePath -> Name -> Version -> String -> Client.Response Client.BodyReader -> IO (Either String ())
writeArchive cache name version hash response =
  writeArchiveHelp
    (WE cache name version hash (Client.responseBody response))
    (WS 0 SHA.sha1Incremental (Binary.runGetIncremental Binary.get))


data WriteEnv =
  WE
    { _cache :: FilePath
    , _name :: Name
    , _version :: Version
    , _hash :: String
    , _body :: Client.BodyReader
    }


data WriteState =
  WS
    { _len :: !Int
    , _sha :: !(Binary.Decoder SHA.SHA1State)
    , _zip :: !(Binary.Decoder Zip.Archive)
    }


writeArchiveHelp :: WriteEnv -> WriteState -> IO (Either String ())
writeArchiveHelp env@(WE cache name version hash body) (WS len sha zip) =
  case zip of
    Binary.Fail _ _ _ ->
      return $ Left "seems like the .zip is corrupted"

    Binary.Partial k ->
      do  chunk <- Client.brRead body
          writeArchiveHelp env $ WS
            { _len = len + BS.length chunk
            , _sha = Binary.pushChunk sha chunk
            , _zip = k (if BS.null chunk then Nothing else Just chunk)
            }

    Binary.Done _ _ archive ->
      let
        realHash =
          SHA.showDigest (SHA.completeSha1Incremental sha len)
      in
        if hash /= realHash then
          return $ Left $ "Expecting hash of content to be " ++ hash ++ ", but it is " ++ realHash
        else
          do  let opts = [Zip.OptDestination (cache </> Pkg.toFilePath name)]
              let vsn = Pkg.versionToString version
              let entries = map (replaceRoot vsn) (Zip.zEntries archive)
              Right <$> mapM_ (Zip.writeEntry opts) entries


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
      [ Multi.partFileSource "documentation.json" "documentation.json"
      , Multi.partFileSource "elm.json" "elm.json"
      , Multi.partFileSource "README.md" "README.md"
      ]
  in
    Http.run $ Http.package "register" params $ \rawRequest manager ->
      do  requestWithBody <- Multi.formDataBody files rawRequest
          let request = requestWithBody { Client.responseTimeout = Nothing }
          void $ Client.httpLbs request manager
          return (Right ())

