{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Task
  ( Task, run, throw, try
  , Env
  , getVersionsPath
  , getPackageCachePath, getPackageInfoPath
  , report
  , interleave
  , fetch, fetchUrl, makeUrl
  )
  where

import qualified Control.Exception as E
import Control.Concurrent.ParallelIO.Local (withPool, parallelInterleaved)
import Control.Monad.Except (ExceptT, runExceptT, catchError, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Network
import qualified Network.HTTP as Http (urlEncodeVars)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.HTTP.Types as Http
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Elm.Assets as Assets
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)
import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress



-- TASKS


type Task =
  ExceptT Error.Error (ReaderT Env IO)


data Env =
  Env
    { _maxConcurrentDownloads :: Int
    , _cacheDirectory :: FilePath
    , _httpManager :: Http.Manager
    , _reporter :: Progress.Reporter
    }


run :: Progress.Reporter -> Task a -> IO (Either Error.Error a)
run reporter task =
  Network.withSocketsDo $
    do  cacheDir <- Assets.getPackageRoot
        httpManager <- Http.newManager Http.tlsManagerSettings
        let env = Env 4 cacheDir httpManager reporter
        runReaderT (runExceptT task) env


throw :: Error.Error -> Task a
throw =
  throwError


try :: a -> Task a -> Task a
try recover task =
  catchError task (\_ -> return recover)



-- CACHING


getVersionsPath :: Task FilePath
getVersionsPath =
  do  cacheDir <- getCacheDirectory
      return (cacheDir </> "versions.dat")


getPackageCachePath :: Name -> Version -> Task FilePath
getPackageCachePath name version =
  do  cacheDir <- getCacheDirectory
      let dir = cacheDir </> Pkg.toFilePath name </> Pkg.versionToString version
      liftIO (createDirectoryIfMissing True dir)
      return dir


getPackageInfoPath :: Name -> Version -> Task FilePath
getPackageInfoPath name version =
  do  cacheDir <- getPackageCachePath name version
      return (cacheDir </> "elm.dat")


getCacheDirectory :: Task FilePath
getCacheDirectory =
  asks _cacheDirectory



-- REPORTER


report :: Progress.Progress -> Task ()
report progress =
  do  reporter <- asks _reporter
      liftIO (reporter progress)



-- THREAD POOL


interleave :: [Task a] -> Task [a]
interleave tasks =
  do  env <- ask

      eithers <-
        liftIO $ withPool (_maxConcurrentDownloads env) $ \pool ->
          parallelInterleaved pool $
            map (\task -> runReaderT (runExceptT task) env) tasks

      either throwError return (sequence eithers)



-- URLs


domain :: String
domain =
  "http://localhost:8008"


makeUrl :: String -> [(String,String)] -> String
makeUrl path params =
  let
    query =
      if null params then
        ""
      else
        "?" ++ Http.urlEncodeVars (versionParam : params)
  in
    domain ++ "/" ++ path ++ query


versionParam :: (String, String)
versionParam =
  ( "elm-package-version"
  , Pkg.versionToString Compiler.version
  )



-- HTTP


fetch :: String -> [(String, String)] -> (Http.Request -> Http.Manager -> IO a) -> Task a
fetch path params handler =
  fetchUrl (makeUrl path params) handler


fetchUrl :: String -> (Http.Request -> Http.Manager -> IO a) -> Task a
fetchUrl url handler =
  do  manager <- asks _httpManager
      result <- liftIO (fetchSafe url manager handler)
      either throwError return result


fetchSafe
  :: String
  -> Http.Manager
  -> (Http.Request -> Http.Manager -> IO a)
  -> IO (Either Error.Error a)
fetchSafe url manager handler =
  fetchUnsafe url manager handler
    `E.catch` handleHttpError url
    `E.catch` \e -> handleAnyError url (e :: E.SomeException)


fetchUnsafe
  :: String
  -> Http.Manager
  -> (Http.Request -> Http.Manager -> IO a)
  -> IO (Either err a)
fetchUnsafe url manager handler =
  do  request <- Http.parseUrlThrow url
      result <- handler request manager
      return (Right result)


handleHttpError :: String -> Http.HttpException -> IO (Either Error.Error b)
handleHttpError url exception =
  case exception of
    Http.StatusCodeException (Http.Status _code err) headers _ ->
      return $ Left $ Error.HttpRequestFailed url $ BSC.unpack $
        case List.lookup "X-Response-Body-Start" headers of
          Just msg | not (BSC.null msg) ->
            msg

          _ ->
            err

    _ ->
      handleAnyError url exception


handleAnyError :: (E.Exception e) => String -> e -> IO (Either Error.Error b)
handleAnyError url exception =
  return $ Left $ Error.HttpRequestFailed url (show exception)

