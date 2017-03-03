module Reporting.Progress
  ( Reporter
  , Progress(..)
  , Outcome(..)
  )
  where


import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import Reporting.Error (Error)



-- PROGRESS


type Reporter =
  Progress -> IO ()


data Progress
  = Start

  -- download packages
  | DownloadStart [(Name, Version)]
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

  -- end
  | Success
  | Failure Error


data Outcome = Good | Bad
