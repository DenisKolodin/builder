{-# OPTIONS_GHC -Wall #-}
module Reporting.Progress.Terminal
  ( create
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import qualified System.Info as System
import System.IO (hFlush, hPutStr, stdout)
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), (<+>), displayIO, green, red, renderPretty, text
  )

import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import qualified Reporting.Error as Error
import Reporting.Progress ( Msg(..), Progress(..), Outcome(..) )
import qualified Reporting.Progress as Progress
import qualified Reporting.Progress.Bar as Bar



-- CREATE


create :: IO Progress.Reporter
create =
  do  chan <- newChan
      mvar <- newEmptyMVar
      forkIO (loop chan (State 0 0 0) >>= putMVar mvar)
      return (Progress.makeReporter chan mvar)



-- STATE


data State =
  State
    { _total :: !Int
    , _good :: !Int
    , _bad :: !Int
    }



-- LOOP


loop :: Chan Msg -> State -> IO ()
loop chan state =
  do  msg <- readChan chan
      case msg of
        End (Just err) ->
          Error.toStderr err

        End Nothing ->
          return ()

        Progress progress ->
          loopHelp chan progress state


loopHelp :: Chan Msg -> Progress -> State -> IO ()
loopHelp chan progress state@(State total good bad) =
  case progress of


    -- DOWNLOADS

    DownloadStart [] ->
      loop chan state

    DownloadStart _ ->
      do  putStrLn "Starting downloads...\n"
          loop chan state

    DownloadPkgStart _name _version ->
      loop chan state

    DownloadPkgEnd name version outcome ->
      do  writeDoc (makeBullet name version outcome)
          loop chan state

    DownloadEnd Bad ->
      do  putStrLn ""
          loop chan state

    DownloadEnd Good ->
      do  putStrLn ""
          loop chan state


    -- BUILD DEPS

    BuildDepsStart size ->
      do  hPutStr stdout "Verifying dependencies..."
          hFlush stdout
          loop chan (State size 0 0)

    BuildDepsProgress ->
      do  let n = good + 1
          let msg = "\rBuilding dependencies (" ++ show n ++ "/" ++ show total ++ ")"
          hPutStr stdout msg
          hFlush stdout
          loop chan (State total n 0)

    BuildDepsEnd ->
      do  putStrLn "\rDependencies ready!                "
          loop chan (State 0 0 0)


    -- COMPILE

    CompileStart size ->
      loop chan (State size 0 0)

    CompileFileStart _ ->
      loop chan state

    CompileFileEnd _ Good ->
      do  hPutStr stdout $ Bar.render (good + 1) bad total
          hFlush stdout
          loop chan (State total (good + 1) bad)

    CompileFileEnd _ Bad ->
      do  hPutStr stdout $ Bar.render good (bad + 1) total
          hFlush stdout
          loop chan (State total good (bad + 1))

    CompileEnd ->
      let
        message =
          case (bad, total) of
            (0, 1) -> "Success! Compiled 1 module."
            (0, n) -> "Success! Compiled " ++ show n ++ " modules."
            (1, _) -> "Detected errors in 1 module."
            (n, _) -> "Detected errors in " ++ show n ++ " modules."
      in
        do  hPutStr stdout $ Bar.clear
            putStrLn message
            loop chan (State 0 0 0)



-- DOC HELPERS


writeDoc :: Doc -> IO ()
writeDoc doc =
  displayIO stdout $ renderPretty 1 80 doc


makeBullet :: Name -> Version -> Outcome -> Doc
makeBullet name version outcome =
  let
    nm =
      text (Pkg.toString name)

    vsn =
      text (Pkg.versionToString version)

    bullet =
      case outcome of
        Good -> good
        Bad -> bad
  in
    text "  " <> bullet <+> nm <+> vsn <> text "\n"


good :: Doc
good =
  green $ text $
    if System.os == "windows" then "+" else "●"


bad :: Doc
bad =
  red $ text $
    if System.os == "windows" then "X" else "✗"
