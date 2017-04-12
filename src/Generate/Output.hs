{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Output
  ( generate
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map ((!))
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Args as Args
import qualified File.Crawl as Crawl
import qualified File.Hash as Hash
import qualified File.IO as IO
import qualified Generate.App as App
import qualified Generate.Plan as Plan
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- GENERATE


generate :: Summary.Summary -> Crawl.Graph () -> Task.Task ()
generate summary graph@(Crawl.Graph args _ _ _ _) =
  case args of
    Args.Pkg _ ->
      return ()

    Args.Roots names ->
      generateMonolith summary graph (NonEmpty.toList names)

    Args.App plan ->
      generatePlan summary graph plan



-- GENERATE MONOLITH


generateMonolith :: Summary.Summary -> Crawl.Graph () -> [Module.Raw] -> Task.Task ()
generateMonolith summary@(Summary.Summary _ project _ _ deps) graph names =
  do
      objectGraph <- organize summary graph
      let pkg = Project.getName project
      let roots = map (Module.Canonical pkg) names
      let table = Obj.symbolTable Map.empty
      let (natives, builder) = Compiler.generate table objectGraph (Obj.mains roots)

      cacheDir <- Task.getPackageCacheDir
      liftIO $ IO.put "elm.js" $
        do  IO.putBuilder App.header
            mapM_ (IO.putFile . pathTo cacheDir deps) natives
            IO.putBuilder builder
            IO.putBuilder (App.footer Nothing roots)



-- ORGANIZE


organize :: Summary.Summary -> Crawl.Graph () -> Task.Task Obj.Graph
organize (Summary.Summary _ _ _ _ deps) (Crawl.Graph _ locals _ _ _) =
  do  localObjs <- Obj.unions <$> traverse loadModuleObj (Map.keys locals)
      foreignObjs <- Obj.unions <$> traverse loadPackageObj (Map.toList deps)
      return (Obj.union localObjs foreignObjs)


loadModuleObj :: Module.Raw -> Task.Task Obj.Graph
loadModuleObj name =
  IO.readBinary (Paths.elmo name)


loadPackageObj :: ( Pkg.Name, (Pkg.Version, deps) ) -> Task.Task Obj.Graph
loadPackageObj ( name, (version,_) ) =
  do  dir <- Task.getPackageCacheDirFor name version
      IO.readBinary (dir </> "objs.dat")



-- GENERATE PLAN


generatePlan :: Summary.Summary -> Crawl.Graph () -> Plan.Plan -> Task.Task ()
generatePlan summary graph plan =
  error "TODO generate based on the plan"



-- KERNEL PATHS


pathTo :: FilePath -> Summary.DepsGraph -> Module.Canonical -> FilePath
pathTo cacheDir deps (Module.Canonical pkg name) =
  cacheDir
  </> Pkg.toFilePath pkg
  </> Pkg.versionToString (fst (deps ! pkg))
  </> "src"
  </> Module.nameToPath name
  <.> "js"
