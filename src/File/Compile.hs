module File.Compile (compileAll) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM, void, when)
import Control.Monad.Except (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg

import Elm.Project (Project)
import qualified Elm.Project as Project
import qualified File.Plan as Plan
import qualified File.IO as IO
import qualified Reporting.Error.Compile as E
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- COMPILE ALL


compileAll
  :: Project
  -> Dict (MVar Module.Interface)
  -> Dict Plan.Info
  -> Task.Task (Dict Compiler.Result)
compileAll project ifaces modules =
  do  answers <- liftIO $ compileAllHelp project ifaces modules
      case sortAnswers answers of
        Left errors ->
          Task.throw (Error.Compile errors)

        Right results ->
          return results


compileAllHelp :: Project -> Dict (MVar Module.Interface) -> Dict Plan.Info -> IO (Dict Answer)
compileAllHelp project ifaces modules =
  do  mvar <- newEmptyMVar
      answerMVars <- Map.traverseWithKey (compile project mvar ifaces) modules
      putMVar mvar answerMVars
      traverse readMVar answerMVars



-- ANSWERS


data Answer
  = Blocked
  | Bad FilePath [Compiler.Error]
  | Good Compiler.Result


type Dict a = Map.Map Module.Raw a


sortAnswers :: Dict Answer -> Either (Dict E.Error) (Dict Compiler.Result)
sortAnswers answers =
  Map.foldlWithKey sortAnswersHelp (Right Map.empty) answers


sortAnswersHelp
  :: Either (Dict E.Error) (Dict Compiler.Result)
  -> Module.Raw
  -> Answer
  -> Either (Dict E.Error) (Dict Compiler.Result)
sortAnswersHelp acc name answer =
  case answer of
    Blocked ->
      acc

    Bad path errors ->
      case acc of
        Left dict ->
          Left (Map.insert name (E.Error path errors) dict)

        Right _ ->
          Left (Map.singleton name (E.Error path errors))

    Good result ->
      case acc of
        Left _ ->
          acc

        Right results ->
          Right (Map.insert name result results)



-- COMPILE


compile
  :: Project
  -> MVar (Dict (MVar Answer))
  -> Dict (MVar Module.Interface)
  -> Module.Raw
  -> Plan.Info
  -> IO (MVar Answer)
compile project answersMVar cachedIfaces name info =
  do  mvar <- newEmptyMVar

      void $ forkIO $
        do  let pkg = Project.getName project
            answers <- readMVar answersMVar
            maybeIfaces <- getIfaces pkg answers cachedIfaces info
            case maybeIfaces of
              Nothing ->
                putMVar mvar Blocked

              Just ifaces ->
                do  let path = Plan._path info
                    let isExposed = elem name (Project.getRoots project)
                    let imports = makeImports info
                    let context = Compiler.Context pkg isExposed imports ifaces
                    source <- IO.readUtf8 path -- TODO store in Plan.Info instead?
                    case Compiler.compile context source of
                      (localizer, warnings, Left errors) ->
                        putMVar mvar (Bad path errors)

                      (localizer, warnings, Right result) ->
                        putMVar mvar (Good result)

      return mvar



-- IMPORTS


makeImports :: Plan.Info -> Dict Module.Canonical
makeImports (Plan.Info _ clean dirty foreign) =
  let
    mkLocal name =
      ( name, Module.Canonical Pkg.dummyName name )

    mkForeign (name, pkg, _vsn) =
      ( name, Module.Canonical pkg name )
  in
    Map.fromList $
      map mkLocal clean
      ++ map mkLocal dirty
      ++ map mkForeign foreign



-- INTERFACES


getIfaces
  :: Pkg.Name
  -> Dict (MVar Answer)
  -> Dict (MVar Module.Interface)
  -> Plan.Info
  -> IO (Maybe Module.Interfaces)
getIfaces pkg answers ifaces (Plan.Info path clean dirty foreign) =
  do  let label name = (name, pkg, ())

      i1 <- traverse (access ifaces) (map label clean)
      i2 <- traverse (access ifaces) foreign
      let cached = Map.fromList (i1 ++ i2)

      i3 <- traverse (access answers) (map label dirty)
      return $ addAnswers cached i3


access :: Dict (MVar a) -> (Module.Raw, Pkg.Name, vsn) -> IO (Module.Canonical, a)
access ifaces (name, pkg, _vsn) =
  case Map.lookup name ifaces of
    Nothing ->
      error "bug in File.Complie.access, please report at <TODO>!"

    Just mvar ->
      (,) (Module.Canonical pkg name) <$> readMVar mvar


addAnswers :: Module.Interfaces -> [(Module.Canonical, Answer)] -> Maybe Module.Interfaces
addAnswers ifaces answers =
  case answers of
    [] ->
      Just ifaces

    (_, Blocked) : _ ->
      Nothing

    (_, Bad _ _) : _ ->
      Nothing

    (name, Good (Compiler.Result _ iface _)) : otherAnswers ->
      addAnswers (Map.insert name iface ifaces) otherAnswers
