module File.Artifacts
  ( ignore
  , write
  )
  where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM, void)
import Control.Monad.Except (liftIO)
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module

import File.Compile (Answer(..))
import qualified Reporting.Error.Compile as E
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- IGNORE


ignore :: Map Module.Raw Answer -> Task.Task (Map Module.Raw Compiler.Result)
ignore answers =
  let
    ignorer _name result =
      return result
  in
    gather ignorer answers



-- WRITE


write :: Map Module.Raw Answer -> Task.Task (Map Module.Raw Compiler.Result)
write answers =
  let
    writer name result@(Compiler.Result _ ifaces objs) =
      do  mvar <- newEmptyMVar
          void $ forkIO $
            do  Binary.encodeFile (Paths.elmi name) ifaces
                Binary.encodeFile (Paths.elmo name) objs
                putMVar mvar result
          return mvar
  in
    do  mvars <- gather writer answers
        liftIO $ traverse readMVar mvars



-- GATHER


gather :: OnGood a -> Map Module.Raw Answer -> Task.Task (Map Module.Raw a)
gather onGood answers =
  do  summary <- liftIO $
        foldM (gatherHelp onGood) (Right Map.empty) (Map.toList answers)

      case summary of
        Left errors ->
          Task.throw (Error.Compile errors)

        Right results ->
          return results


type OnGood a = Module.Raw -> Compiler.Result -> IO a


gatherHelp :: OnGood a -> Summary a -> (Module.Raw, Answer) -> IO (Summary a)
gatherHelp onGood summary (name, answer) =
  case answer of
    Blocked ->
      return summary

    Bad path src localizer errors ->
      do  let err = E.Error path src localizer errors
          return (addErr name err summary)

    Good result ->
      do  value <- onGood name result
          return (addOk name value summary)



-- DICT RESULT


type Summary a =
  Either (Map Module.Raw E.Error) (Map Module.Raw a)


addOk :: Module.Raw -> a -> Summary a -> Summary a
addOk name result acc =
  case acc of
    Left _ ->
      acc

    Right results ->
      Right (Map.insert name result results)


addErr :: Module.Raw -> E.Error -> Summary a -> Summary a
addErr name err acc =
  case acc of
    Left errors ->
      Left (Map.insert name err errors)

    Right _ ->
      Left (Map.singleton name err)
