{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Website
  ( getElmJson
  , getDocs
  , getNewPackages
  , getAllPackages
  , download
  , githubCommit
  , githubDownload
  , Sha
  , register
  )
  where

import Prelude hiding (zip)
import qualified Codec.Archive.Zip as Zip
import Control.Monad (void)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import qualified Network.HTTP.Types as Http
import qualified System.Directory as Dir
import System.FilePath ((</>), splitFileName)

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg
import qualified Json.Decode as Decode

import qualified Reporting.Error.Http as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Reporting.Task.Http as Http
import qualified Stuff.Paths as Path



-- GET PACKAGE INFO


getElmJson :: Name -> Version -> Task.Task LBS.ByteString
getElmJson name version =
  Http.run $ fetchByteString $
    "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/elm.json"


getDocs :: Name -> Version -> Task.Task LBS.ByteString
getDocs name version =
  Http.run $ fetchByteString $
    "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/docs.json"



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
              Decode.fail "Expecting a new package like \"elm-lang/core@6.0.1\""

        _ ->
          Decode.fail "Expecting a new package like \"elm-lang/core@6.0.1\""



-- ALL PACKAGES


getAllPackages :: Task.Task (Map.Map Name [Version])
getAllPackages =
  Http.run $ fetchJson allPkgsDecoder "all-packages"


allPkgsDecoder :: Decode.Decoder (Map.Map Name [Version])
allPkgsDecoder =
  let
    checkKeys pairs pkgs =
      case pairs of
        [] ->
          Decode.succeed pkgs

        (key, versions) : rest ->
          case Pkg.fromText key of
            Left msg ->
              Decode.fail $ "Field \"" ++ Text.unpack key ++ "\" is not a valid package name. " ++ msg

            Right pkg ->
              checkKeys rest (Map.insert pkg versions pkgs)
  in
    do  dict <- Decode.dict (Decode.list Pkg.versionDecoder)
        checkKeys (HashMap.toList dict) Map.empty



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

          Left jsonProblem ->
            return $ Left $ E.BadJson jsonProblem



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
      "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/endpoint.json"
  in
    Http.andThen (fetchJson endpointDecoder endpointUrl) $ \(endpoint, hash) ->
      let
        start = Progress.DownloadPkgStart name version
        toEnd = Progress.DownloadPkgEnd name version
      in
        Http.report start toEnd $
          Http.anything endpoint $ \request manager ->
            Client.withResponse request manager (downloadArchive cache name version hash)


endpointDecoder :: Decode.Decoder (String, String)
endpointDecoder =
  Decode.map2 (,)
    (Decode.field "url" Decode.string)
    (Decode.field "hash" Decode.string)



-- DOWNLOAD ZIP ARCHIVE


downloadArchive :: FilePath -> Name -> Version -> String -> Client.Response Client.BodyReader -> IO (Either E.Error ())
downloadArchive cache name version expectedHash response =
  do  result <- readArchive (Client.responseBody response) initialArchiveState
      case result of
        Left msg ->
          return (Left msg)

        Right (sha, archive) ->
          if expectedHash == SHA.showDigest sha then
            Right <$> writeArchive archive (cache </> Pkg.toFilePath name) (Pkg.versionToString version)
          else
            return $ Left $ E.BadZipSha expectedHash (SHA.showDigest sha)



-- READ ARCHIVE


data ArchiveState =
  AS
    { _len :: !Int
    , _sha :: !(Binary.Decoder SHA.SHA1State)
    , _zip :: !(Binary.Decoder Zip.Archive)
    }


initialArchiveState :: ArchiveState
initialArchiveState =
  AS 0 SHA.sha1Incremental (Binary.runGetIncremental Binary.get)


type Sha = SHA.Digest SHA.SHA1State


readArchive :: Client.BodyReader -> ArchiveState -> IO (Either E.Error (Sha, Zip.Archive))
readArchive body (AS len sha zip) =
  case zip of
    Binary.Fail _ _ _ ->
      return $ Left E.BadZipData

    Binary.Partial k ->
      do  chunk <- Client.brRead body
          readArchive body $ AS
            { _len = len + BS.length chunk
            , _sha = Binary.pushChunk sha chunk
            , _zip = k (if BS.null chunk then Nothing else Just chunk)
            }

    Binary.Done _ _ archive ->
      return $ Right ( SHA.completeSha1Incremental sha len, archive )



-- WRITE ARCHIVE


writeArchive :: Zip.Archive -> FilePath -> FilePath -> IO ()
writeArchive archive destination newRoot =
  do  Dir.createDirectoryIfMissing True destination
      let opts = [Zip.OptDestination destination]
      mapM_ (Zip.writeEntry opts . replaceRoot newRoot) (Zip.zEntries archive)


replaceRoot :: String -> Zip.Entry -> Zip.Entry
replaceRoot root entry =
  let
    rootless =
      dropWhile (/='/') (Zip.eRelativePath entry)
  in
    entry { Zip.eRelativePath = root ++ rootless }



-- FIND TAGGED COMMIT ON GITHUB


githubCommit :: Name -> Version -> Task.Task String
githubCommit name version =
  let
    endpoint =
      "https://api.github.com/repos/" ++ Pkg.toUrl name ++ "/git/refs/tags/temp-" ++ Pkg.versionToString version

    headers =
      [ ( Http.hUserAgent, "elm-cli" )
      , ( Http.hAccept, "application/json" )
      ]

    decoder =
      Decode.at ["object","sha"] Decode.string
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      do  response <- Client.httpLbs (request { Client.requestHeaders = headers }) manager
          case Decode.parse decoder (Client.responseBody response) of
            Right value ->
              return $ Right value

            Left jsonProblem ->
              return $ Left $ E.BadJson jsonProblem



-- DOWNLOAD FROM GITHUB


githubDownload :: Name -> Version -> FilePath -> Task.Task Sha
githubDownload name version dir =
  let
    endpoint =
      "https://github.com/" ++ Pkg.toUrl name ++ "/zipball/temp-" ++ Pkg.versionToString version ++ "/"
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      Client.withResponse request manager (githubDownloadHelp dir)


githubDownloadHelp :: FilePath -> Client.Response Client.BodyReader -> IO (Either E.Error Sha)
githubDownloadHelp targetDir response =
  do  result <- readArchive (Client.responseBody response) initialArchiveState
      case result of
        Left msg ->
          return (Left msg)

        Right (sha, archive) ->
          do  let (dir, root) = splitFileName targetDir
              writeArchive archive dir root
              return $ Right sha



-- REGISTER PACKAGES


register :: Name -> Version -> String -> Sha -> Task.Task ()
register name version commitHash digest =
  let
    params =
      [ ("name", Pkg.toString name)
      , ("version", Pkg.versionToString version)
      , ("commit-hash", commitHash)
      ]

    files =
      [ Multi.partFileSource "elm.json" "elm.json"
      , Multi.partFileSource "docs.json" Path.docs
      , Multi.partFileSource "README.md" "README.md"
      , Multi.partFileRequestBody "github-hash" "github-hash" $
          Client.RequestBodyBS (BS.pack (SHA.showDigest digest))
      ]
  in
    Http.run $ Http.package "register" params $ \rawRequest manager ->
      do  requestWithBody <- Multi.formDataBody files rawRequest
          let request = requestWithBody { Client.responseTimeout = Client.responseTimeoutNone }
          void $ Client.httpLbs request manager
          return $ Right ()
