module Reporting.Progress.Terminal
  ( create
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import System.IO (hFlush, hPutStr, stdout)
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), (<+>), displayIO, green, red, renderPretty, text
  )

import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import Reporting.Progress ( Progress(..), Outcome(..) )
import qualified Reporting.Progress as Progress
import qualified Reporting.Progress.Bar as Bar



-- CREATE


create :: IO Progress.Reporter
create =
  do  chan <- newChan

      let stepper state =
            do  progress <- readChan chan
                newState <- step progress state
                stepper newState

      forkIO (stepper (State 0 0 0))

      return (writeChan chan)



-- STATE


data State =
  State
    { _total :: !Int
    , _good :: !Int
    , _bad :: !Int
    }



-- STEPPER


step :: Progress -> State -> IO State
step progress state@(State total good bad) =
  case progress of
    Start ->
      return state


    -- DOWNLOADS

    DownloadStart [] ->
      return state

    DownloadStart _ ->
      do  putStrLn "Starting downloads...\n"
          return state

    DownloadPkgStart _name _version ->
      return state

    DownloadPkgEnd name version outcome ->
      do  writeDoc (makeBullet name version outcome)
          return state

    DownloadEnd Bad ->
      do  putStrLn ""
          return state

    DownloadEnd Good ->
      do  putStrLn "\nPackages configured successfully!"
          return state


    -- COMPILE

    CompileStart names ->
      return $ State (length names) 0 0

    CompileFileStart _ ->
      return state

    CompileFileEnd _ Good ->
      do  hPutStr stdout $ Bar.clear
          hPutStr stdout $ Bar.render (good + 1) bad total
          hFlush stdout
          return $ State total (good + 1) bad

    CompileFileEnd _ Bad ->
      do  hPutStr stdout $ Bar.clear
          hPutStr stdout $ Bar.render good (bad + 1) total
          hFlush stdout
          return $ State total good (bad + 1)

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
            return $ State 0 0 0


    -- REAL END

    Success ->
      return state

    Failure _ ->
      return state



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
        Good ->
          green (text "●")

        Bad ->
          red (text "✗")
  in
    text "  " <> bullet <+> nm <+> vsn <> text "\n"
