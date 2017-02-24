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
import System.Directory (doesFileExist, getModificationTime, removeFile)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Deps.Paths
import qualified File.Crawl as Crawl
import qualified Reporting.Task as Task
import qualified Stuff.Paths



-- PLAN


plan :: Crawl.Graph -> Task.Task (Dict Info, Dict (MVar Module.Interface))
plan graph =
  do  pkgDir <- Task.getPackageCacheDir
      liftIO $ planHelp pkgDir graph


type Dict value = Map.Map Module.Raw value


planHelp :: FilePath -> Crawl.Graph -> IO (Dict Info, Dict (MVar Module.Interface))
planHelp pkgDir (Crawl.Graph locals _ foreigns _) =
  do  queue <- newChan

      mvar <- newEmptyMVar
      statusMVars <- Map.traverseWithKey (getStatus mvar queue foreigns) locals
      putMVar mvar statusMVars

      void $ forkIO $
        do  graph <- Map.traverseMaybeWithKey (\_ -> readMVar) statusMVars
            writeChan queue (EndLoop graph)

      ifaceLoader pkgDir queue Map.empty



-- STATUS


type Status = Maybe Info
  -- Nothing == clean
  -- Just info == dirty


data Info =
  Info
    { _path :: FilePath
    , _clean :: [Module.Raw]
    , _dirty :: [Module.Raw]
    , _foreign :: [(Module.Raw, Pkg.Name, Pkg.Version)]
    }



-- GET STATUS


getStatus
  :: MVar (Dict (MVar Status))
  -> Chan Msg
  -> Dict (Pkg.Name, Pkg.Version)
  -> Module.Raw
  -> Crawl.Info
  -> IO (MVar Status)
getStatus statusMVars queue foreigns name (Crawl.Info path deps) =
  do  mvar <- newEmptyMVar

      void $ forkIO $ putMVar mvar =<<
        do  statuses <- readMVar statusMVars
            info <- foldM (addDep statuses foreigns) (Info path [] [] []) deps

            case _dirty info of
              _ : _ ->
                do  getInterfaces queue name info
                    return (Just info)

              [] ->
                do  fresh <- isFresh name path
                    if fresh then return Nothing else
                      do  getInterfaces queue name info
                          return (Just info)

      return mvar


addDep :: Dict (MVar Status) -> Dict (Pkg.Name, Pkg.Version) -> Info -> Module.Raw -> IO Info
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
        Just (pkg, vsn) ->
          return $ info { _foreign = (name, pkg, vsn) : _foreign info }

        Nothing ->
          return info -- must be native



-- IS FRESH


isFresh :: Module.Raw -> FilePath -> IO Bool
isFresh name path =
  do  let elmi = Stuff.Paths.elmi name
      andM
        [ doesFileExist elmi
        , do  cacheTime <- getModificationTime elmi
              srcTime <- getModificationTime path
              return (cacheTime >= srcTime)
        ]


andM :: [IO Bool] -> IO Bool
andM checks =
  case checks of
    [] ->
      return True

    check : otherChecks ->
      do  bool <- check
          if bool then andM otherChecks else return False



-- INTERFACES


getInterfaces :: Chan Msg -> Module.Raw -> Info -> IO ()
getInterfaces queue name info =
  do  -- load available interfaces
      mapM_ (writeChan queue . GetLocal) (_clean info)
      mapM_ (writeChan queue . GetForeign) (_foreign info)

      -- remove existing interface if exists
      let elmi = Stuff.Paths.elmi name
      exists <- doesFileExist elmi
      when exists (removeFile elmi)



-- INTERFACE LOADER


data Msg
  = EndLoop (Dict Info)
  | GetLocal Module.Raw
  | GetForeign (Module.Raw, Pkg.Name, Pkg.Version)


ifaceLoader
  :: FilePath
  -> Chan Msg
  -> Dict (MVar Module.Interface)
  -> IO (Dict Info, Dict (MVar Module.Interface))
ifaceLoader pkgDir queue ifaces =
  do  msg <- readChan queue
      case msg of
        EndLoop dirty ->
          return ( dirty, ifaces )

        GetLocal name ->
          do  let path = Stuff.Paths.elmi name
              newIfaces <- load name path ifaces
              ifaceLoader pkgDir queue newIfaces

        GetForeign (name, pkg, vsn) ->
          do  let path = Deps.Paths.elmi pkgDir pkg vsn name
              newIfaces <- load name path ifaces
              ifaceLoader pkgDir queue newIfaces


load
  :: Module.Raw
  -> FilePath
  -> Dict (MVar Module.Interface)
  -> IO (Dict (MVar Module.Interface))
load name path ifaces =
  case Map.lookup name ifaces of
    Just _ ->
      return ifaces

    Nothing ->
      do  mvar <- newEmptyMVar
          void $ forkIO $ putMVar mvar =<< Binary.decodeFile path
          return $ Map.insert name mvar ifaces
