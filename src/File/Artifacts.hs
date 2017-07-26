module File.Artifacts
  ( ignore
  , write
  , writeDocs
  )
  where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM, void)
import Control.Monad.Except (liftIO)
import qualified Data.Binary as Binary
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs
import qualified Json.Encode as Encode

import File.Compile (Answer(..))
import qualified Reporting.Error.Compile as E
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- IGNORE


ignore :: Map Module.Raw Answer -> Task.Task (Map Module.Raw Compiler.Result)
ignore answers =
  let
    ignorer _name result =
      return result
  in
    gather ignorer answers



-- WRITE


write :: FilePath -> Map Module.Raw Answer -> Task.Task (Map Module.Raw Compiler.Result)
write root answers =
  let
    writer name result@(Compiler.Result _ ifaces objs) =
      do  mvar <- newEmptyMVar
          void $ forkIO $
            do  Binary.encodeFile (Path.elmi root name) ifaces
                Binary.encodeFile (Path.elmo root name) objs
                putMVar mvar result
          return mvar
  in
    do  mvars <- gather writer answers
        liftIO $ traverse readMVar mvars


writeDocs :: FilePath -> Map Module.Raw Compiler.Result -> Task.Task Docs.Documentation
writeDocs path results =
  let
    getDocs (Compiler.Result docs _ _) =
      docs
  in
    case Maybe.mapMaybe getDocs (Map.elems results) of
      [] ->
        return Map.empty

      docs ->
        do  liftIO $ Encode.write path $ Encode.list Docs.encode docs
            return $ Docs.toDict docs



-- GATHER


gather :: OnGood a -> Map Module.Raw Answer -> Task.Task (Map Module.Raw a)
gather onGood answers =
  do  summary <- liftIO $
        foldM (gatherHelp onGood) (Right Map.empty) (Map.toList answers)

      case summary of
        Left (err :| errors) ->
          Task.throw (Error.Compile err errors)

        Right results ->
          return results


type OnGood a = Module.Raw -> Compiler.Result -> IO a


gatherHelp :: OnGood a -> Summary a -> (Module.Raw, Answer) -> IO (Summary a)
gatherHelp onGood summary (name, answer) =
  case answer of
    Blocked ->
      return summary

    Bad path time src localizer errors ->
      do  let err = E.Error name path time src localizer errors
          return (addErr err summary)

    Good result ->
      do  value <- onGood name result
          return (addOk name value summary)



-- DICT RESULT


type Summary a =
  Either (NonEmpty E.Error) (Map Module.Raw a)


addOk :: Module.Raw -> a -> Summary a -> Summary a
addOk name result acc =
  case acc of
    Left _ ->
      acc

    Right results ->
      Right (Map.insert name result results)


addErr :: E.Error -> Summary a -> Summary a
addErr err acc =
  case acc of
    Left errors ->
      Left (NonEmpty.cons err errors)

    Right _ ->
      Left (err :| [])
