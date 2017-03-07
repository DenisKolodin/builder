{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Task
  ( Task, Task_, run, throw, mapError
  , Env
  , getPackageCacheDir
  , getPackageCacheDirFor
  , report
  , getReporter
  , getSilentRunner
  , workerMVar
  , workerChan
  , fetch, fetchFromInternet, makeUrl
  )
  where

import qualified Control.Exception as E
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Monad (forever, join, replicateM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
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
  Task_ Error.Error


type Task_ e =
  ExceptT e (ReaderT Env IO)


data Env =
  Env
    { _cacheDir :: FilePath
    , _worker :: IO () -> IO ()
    , _httpManager :: Http.Manager
    , _reporter :: Progress.Reporter
    }


run :: Progress.Reporter -> Task a -> IO (Either Error.Error a)
run reporter task =
  Network.withSocketsDo $
    do  root <- Assets.getPackageRoot
        pool <- initPool 4
        httpManager <- Http.newManager Http.tlsManagerSettings
        let env = Env root pool httpManager reporter
        runReaderT (runExceptT task) env


throw :: e -> Task_ e a
throw =
  throwError


mapError :: (x -> y) -> Task_ x a -> Task_ y a
mapError =
  withExceptT



-- CACHING


getPackageCacheDir :: Task_ e FilePath
getPackageCacheDir =
  asks _cacheDir


getPackageCacheDirFor :: Name -> Version -> Task_ e FilePath
getPackageCacheDirFor name version =
  do  cacheDir <- getPackageCacheDir
      let dir = cacheDir </> Pkg.toFilePath name </> Pkg.versionToString version
      liftIO (createDirectoryIfMissing True dir)
      return dir



-- REPORTER


report :: Progress.Progress -> Task_ e ()
report progress =
  do  reporter <- asks _reporter
      liftIO (reporter progress)


getReporter :: Task Progress.Reporter
getReporter =
  asks _reporter



-- RUNNER


getSilentRunner :: Task_ x (Task_ e a -> IO (Either e a))
getSilentRunner =
  do  env <- ask
      let silentEnv = env { _reporter = \_ -> return () }
      return $ \task -> runReaderT (runExceptT task) silentEnv



-- THREAD POOL


workerMVar :: Task_ e a -> Task_ x (MVar (Either e a))
workerMVar task =
  do  env <- ask
      mvar <- liftIO newEmptyMVar

      liftIO $ _worker env $
        do  result <- runReaderT (runExceptT task) env
            putMVar mvar result

      return mvar


workerChan :: Chan (Either e a) -> Task_ e a -> Task_ x ()
workerChan chan task =
  do  env <- ask

      liftIO $ _worker env $
        do  result <- runReaderT (runExceptT task) env
            writeChan chan result


initPool :: Int -> IO (IO () -> IO ())
initPool size =
  do  chan <- newChan

      replicateM_ size $ forkIO $ forever $
        join (readChan chan)

      return $ writeChan chan



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


type Handler a = Http.Request -> Http.Manager -> IO (Either String a)


fetch :: String -> [(String, String)] -> Handler a -> Task a
fetch path params handler =
  fetchFromInternet (makeUrl path params) handler


fetchFromInternet :: String -> Handler a -> Task a
fetchFromInternet url handler =
  do  manager <- asks _httpManager
      result <- liftIO (fetchSafe url manager handler)
      either throwError return result


fetchSafe :: String -> Http.Manager -> Handler a -> IO (Either Error.Error a)
fetchSafe url manager handler =
  fetchUnsafe url manager handler
    `E.catch` handleHttpError url
    `E.catch` \e -> handleAnyError url (e :: E.SomeException)


fetchUnsafe :: String -> Http.Manager -> Handler a -> IO (Either Error.Error a)
fetchUnsafe url manager handler =
  do  request <- Http.parseUrlThrow url
      result <- handler request manager
      case result of
        Right value ->
          return (Right value)

        Left msg ->
          return (Left (Error.HttpRequestFailed url msg))


handleHttpError :: String -> Http.HttpException -> IO (Either Error.Error a)
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


handleAnyError :: (E.Exception e) => String -> e -> IO (Either Error.Error a)
handleAnyError url exception =
  return $ Left $ Error.HttpRequestFailed url (show exception)

