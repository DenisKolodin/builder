{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Task
  ( Task, run, throw, try
  , Env
  , getPackageCacheDir
  , getPackageCacheDirFor
  , report
  , interleave
  , fetch, makeUrl
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
    , _cacheDir :: FilePath
    , _httpManager :: Http.Manager
    , _reporter :: Progress.Reporter
    }


run :: Progress.Reporter -> Task a -> IO (Either Error.Error a)
run reporter task =
  Network.withSocketsDo $
    do  root <- Assets.getPackageRoot
        httpManager <- Http.newManager Http.tlsManagerSettings
        let env = Env 4 root httpManager reporter
        runReaderT (runExceptT task) env


throw :: Error.Error -> Task a
throw =
  throwError


try :: a -> Task a -> Task a
try recover task =
  catchError task (\_ -> return recover)



-- CACHING


getPackageCacheDir :: Task FilePath
getPackageCacheDir =
  asks _cacheDir


getPackageCacheDirFor :: Name -> Version -> Task FilePath
getPackageCacheDirFor name version =
  do  cacheDir <- getPackageCacheDir
      let dir = cacheDir </> Pkg.toFilePath name </> Pkg.versionToString version
      liftIO (createDirectoryIfMissing True dir)
      return dir



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
  "http://localhost:8000"


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


fetch
  :: String
  -> [(String, String)]
  -> (Http.Request -> Http.Manager -> IO (Either String a))
  -> Task a
fetch path params handler =
  do  let url = makeUrl path params
      manager <- asks _httpManager
      result <- liftIO (fetchSafe url manager handler)
      either throwError return result


fetchSafe
  :: String
  -> Http.Manager
  -> (Http.Request -> Http.Manager -> IO (Either String a))
  -> IO (Either Error.Error a)
fetchSafe url manager handler =
  fetchUnsafe url manager handler
    `E.catch` handleHttpError url
    `E.catch` \e -> handleAnyError url (e :: E.SomeException)


fetchUnsafe
  :: String
  -> Http.Manager
  -> (Http.Request -> Http.Manager -> IO (Either String a))
  -> IO (Either Error.Error a)
fetchUnsafe url manager handler =
  do  request <- Http.parseUrlThrow url
      result <- handler request manager
      case result of
        Right value ->
          return (Right value)

        Left msg ->
          return (Left (Error.HttpRequestFailed url msg))


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

