module File.Compile (compileAll) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import Elm.Project (Project)
import qualified Elm.Project as Project
import qualified File.Plan as Plan
import qualified File.IO as IO
import qualified Reporting.Error.Compile as E
import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task



-- COMPILE ALL


compileAll
  :: Project
  -> Module.Interfaces
  -> Dict Plan.Info
  -> Task.Task (Dict Compiler.Result)
compileAll project cachedIfaces modules =
  do  Task.report (Progress.CompileStart (Map.size modules))

      reporter <- Task.getReporter

      answers <- liftIO $
        do  mvar <- newEmptyMVar
            ifaces <- newMVar cachedIfaces
            answerMVars <- Map.traverseWithKey (compile reporter project mvar ifaces) modules
            putMVar mvar answerMVars
            traverse readMVar answerMVars

      Task.report Progress.CompileEnd

      case sortAnswers answers of
        Left errors ->
          Task.throw (Error.Compile errors)

        Right results ->
          return results



-- ANSWERS


data Answer
  = Blocked
  | Bad FilePath Text [Compiler.Error]
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

    Bad path src errors ->
      case acc of
        Left dict ->
          Left (Map.insert name (E.Error path src errors) dict)

        Right _ ->
          Left (Map.singleton name (E.Error path src errors))

    Good result ->
      case acc of
        Left _ ->
          acc

        Right results ->
          Right (Map.insert name result results)



-- COMPILE


compile
  :: Progress.Reporter
  -> Project
  -> MVar (Dict (MVar Answer))
  -> MVar Module.Interfaces
  -> Module.Raw
  -> Plan.Info
  -> IO (MVar Answer)
compile reporter project answersMVar ifacesMVar name info =
  do  mvar <- newEmptyMVar

      void $ forkIO $
        do  answers <- readMVar answersMVar
            blocked <- isBlocked answers info
            if blocked
              then putMVar mvar Blocked
              else
                do  reporter (Progress.CompileFileStart name)
                    let pkg = Project.getName project
                    let path = Plan._path info
                    let isExposed = elem name (Project.getRoots project)
                    let imports = makeImports project info
                    ifaces <- readMVar ifacesMVar
                    let context = Compiler.Context pkg isExposed imports ifaces
                    source <- IO.readUtf8 path -- TODO store in Plan.Info instead?
                    case Compiler.compile context source of
                      (localizer, warnings, Left errors) ->
                        do  reporter (Progress.CompileFileEnd name Progress.Bad)
                            putMVar mvar (Bad path source errors)

                      (localizer, warnings, Right result@(Compiler.Result _ iface _)) ->
                        do  reporter (Progress.CompileFileEnd name Progress.Good)
                            let canonicalName = Module.Canonical pkg name
                            lock <- takeMVar ifacesMVar
                            putMVar ifacesMVar (Map.insert canonicalName iface lock)
                            putMVar mvar (Good result)

      return mvar



-- IMPORTS


makeImports :: Project -> Plan.Info -> Dict Module.Canonical
makeImports project (Plan.Info _ clean dirty foreign) =
  let
    pkgName =
      Project.getName project

    mkLocal name =
      ( name, Module.Canonical pkgName name )

    mkForeign canonicalName@(Module.Canonical _ name) =
      ( name, canonicalName )
  in
    Map.fromList $
      map mkLocal clean
      ++ map mkLocal dirty
      ++ map mkForeign foreign



-- INTERFACES


isBlocked :: Dict (MVar Answer) -> Plan.Info -> IO Bool
isBlocked answers info =
  anyBlock <$> traverse (get answers) (Plan._dirty info)


get :: Dict (MVar Answer) -> Module.Raw -> IO Answer
get names name =
  case Map.lookup name names of
    Nothing ->
      error "bug manifesting in File.Complie.get, please report at <TODO>!"

    Just mvar ->
      readMVar mvar


anyBlock :: [Answer] -> Bool
anyBlock answers =
  case answers of
    [] ->
      False

    Blocked : _ ->
      True

    Bad _ _ _ : _ ->
      True

    Good _ : otherAnswers ->
      anyBlock otherAnswers
