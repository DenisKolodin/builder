{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Plan
  ( plan
  , Info(..)
  )
  where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM, void, when)
import Control.Monad.Except (liftIO)
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Crawl as Crawl
import qualified Reporting.Task as Task
import qualified Stuff.Paths



-- PLAN


type Dict value = Map.Map Module.Raw value


plan :: Summary.Summary -> Crawl.Graph () -> Task.Task (Dict Info, Module.Interfaces)
plan (Summary.Summary root project _ ifaces _) (Crawl.Graph locals _ foreigns _) =
  liftIO $
  do  queue <- newChan
      let env = Env queue root (Project.getName project)

      mvar <- newEmptyMVar
      statusMVars <- Map.traverseWithKey (getStatus env mvar foreigns) locals
      putMVar mvar statusMVars

      void $ forkIO $
        do  graph <- Map.traverseMaybeWithKey (\_ -> readMVar) statusMVars
            writeChan queue (EndLoop graph)

      ifaceLoader queue ifaces


data Env =
  Env
    { _queue :: Chan Msg
    , _root :: FilePath
    , _pkg :: Pkg.Name
    }



-- STATUS


type Status = Maybe Info
  -- Nothing == clean
  -- Just info == dirty


data Info =
  Info
    { _path :: FilePath
    , _src :: Text.Text
    , _clean :: [Module.Raw]
    , _dirty :: [Module.Raw]
    , _foreign :: [Module.Canonical]
    }



-- GET STATUS


getStatus
  :: Env
  -> MVar (Dict (MVar Status))
  -> Dict Pkg.Package
  -> Module.Raw
  -> Crawl.Info
  -> IO (MVar Status)
getStatus env statusMVars foreigns name (Crawl.Info path src deps) =
  do  mvar <- newEmptyMVar

      void $ forkIO $ putMVar mvar =<<
        do  statuses <- readMVar statusMVars
            info <- foldM (addDep statuses foreigns) (Info path src [] [] []) deps

            let elmi = _root env </> Stuff.Paths.elmi name

            case _dirty info of
              _ : _ ->
                do  remove elmi
                    return (Just info)

              [] ->
                do  fresh <- isFresh path elmi
                    if fresh
                      then
                        do  let canonical = Module.Canonical (_pkg env) name
                            writeChan (_queue env) (Get canonical elmi)
                            return Nothing
                      else
                        do  remove elmi
                            return (Just info)

      return mvar


addDep :: Dict (MVar Status) -> Dict Pkg.Package -> Info -> Module.Raw -> IO Info
addDep locals foreigns info name =
  case Map.lookup name locals of
    Just mvar ->
      do  status <- readMVar mvar
          case status of
            Nothing ->
              return $ info { _clean = name : _clean info }

            Just _ ->
              return $ info { _dirty = name : _dirty info }

    Nothing ->
      case Map.lookup name foreigns of
        Just (Pkg.Package pkg _vsn) ->
          return $ info { _foreign = Module.Canonical pkg name : _foreign info }

        Nothing ->
          return info -- must be native



-- IS FRESH


isFresh :: FilePath -> FilePath -> IO Bool
isFresh path elmi =
  andM
    [ doesFileExist elmi
    , do  elmiTime <- getModificationTime elmi
          srcTime <- getModificationTime path
          return (elmiTime >= srcTime)
    ]


andM :: [IO Bool] -> IO Bool
andM checks =
  case checks of
    [] ->
      return True

    check : otherChecks ->
      do  bool <- check
          if bool then andM otherChecks else return False


remove :: FilePath -> IO ()
remove path =
  do  exists <- doesFileExist path
      when exists (removeFile path)



-- INTERFACE LOADER


data Msg
  = EndLoop (Dict Info)
  | Get Module.Canonical FilePath


ifaceLoader :: Chan Msg -> Module.Interfaces -> IO (Dict Info, Module.Interfaces)
ifaceLoader queue ifaces =
  do  msg <- readChan queue
      case msg of
        EndLoop dirty ->
          return ( dirty, ifaces )

        Get canonical elmi ->
          case Map.lookup canonical ifaces of
            Just _ ->
              ifaceLoader queue ifaces

            Nothing ->
              do  iface <- Binary.decodeFile elmi
                  ifaceLoader queue $ Map.insert canonical iface ifaces
