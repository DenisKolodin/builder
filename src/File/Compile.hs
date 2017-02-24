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
import qualified Reporting.Task as Task



-- COMPILE ALL


compileAll
  :: Project
  -> Dict (MVar Module.Interface)
  -> Dict Plan.Info
  -> Task.Task ()
compileAll project ifaces modules =
  do  answers <- liftIO $ compileAllHelp project ifaces modules
      error "TODO"


type Dict a = Map.Map Module.Raw a

data Answer
  = Blocked
  | Bad [Compiler.Error]
  | Good Compiler.Result


compileAllHelp :: Project -> Dict (MVar Module.Interface) -> Dict Plan.Info -> IO (Dict Answer)
compileAllHelp project ifaces modules =
  do  mvar <- newEmptyMVar
      answerMVars <- Map.traverseWithKey (compile project mvar ifaces) modules
      putMVar mvar answerMVars
      traverse readMVar answerMVars



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
                do  let isExposed = elem name (Project.getRoots project)
                    let imports = makeImports info
                    let context = Compiler.Context pkg isExposed imports ifaces
                    source <- IO.readUtf8 (Plan._path info) -- TODO store in Plan.Info instead?
                    case Compiler.compile context source of
                      (localizer, warnings, Left errors) ->
                        putMVar mvar (Bad errors)

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

    (_, Bad _) : _ ->
      Nothing

    (name, Good (Compiler.Result _ iface _)) : otherAnswers ->
      addAnswers (Map.insert name iface ifaces) otherAnswers
