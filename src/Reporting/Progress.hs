{-# OPTIONS_GHC -Wall #-}
module Reporting.Progress
  ( Reporter(..)
  , makeReporter
  , Msg(..)
  , Progress(..)
  , Outcome(..)
  )
  where


import Control.Concurrent.Chan (Chan, writeChan)
import Control.Concurrent.MVar (MVar, readMVar)
import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)
import Reporting.Error (Error)



-- REPORTER


data Reporter =
  Reporter
    { _tell :: Progress -> IO ()
    , _end :: Maybe Error -> IO ()
    }


makeReporter :: Chan Msg -> MVar () -> Reporter
makeReporter chan mvar =
  let
    tell progress =
      writeChan chan (Progress progress)

    end maybeError =
      do  writeChan chan (End maybeError)
          readMVar mvar
  in
    Reporter tell end



-- MESSAGES


data Msg = Progress Progress | End (Maybe Error)


data Progress
  -- download packages
  = DownloadStart [(Name, Version)]
  | DownloadPkgStart Name Version
  | DownloadPkgEnd Name Version Outcome
  | DownloadEnd Outcome

  -- build dependencies
  | BuildDepsStart Int
  | BuildDepsProgress
  | BuildDepsEnd

  -- compile files
  | CompileStart Int
  | CompileFileStart Module.Raw
  | CompileFileEnd Module.Raw Outcome
  | CompileEnd


data Outcome = Good | Bad
