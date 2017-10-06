{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Output
  ( generate
  , generateReplFile
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Package as Pkg

import qualified Elm.Project.Flags as Flags
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Args as Args
import qualified File.Crawl as Crawl
import qualified File.IO as IO
import qualified Generate.App as App
import qualified Generate.Repl as Repl
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- GENERATE


generate :: Flags.Options -> Summary.Summary -> Crawl.Graph () -> Task.Task ()
generate options summary graph@(Crawl.Graph args _ _ _ _) =
  case args of
    Args.Pkg _ ->
      return ()

    Args.Roots names ->
      generateMonolith options summary graph (NonEmpty.toList names)



-- GENERATE MONOLITH


generateMonolith :: Flags.Options -> Summary.Summary -> Crawl.Graph () -> [Module.Raw] -> Task.Task ()
generateMonolith (Flags.Options debug target output) summary@(Summary.Summary _ project _ _ _) graph names =
  do
      objectGraph <- organize summary graph
      let pkg = Project.getName project
      let roots = map (Module.Canonical pkg) names
      let builder = Compiler.generate debug target objectGraph (Obj.mains roots)

      let write path =
            IO.put path $
              do  IO.putBuilder App.header
                  IO.putBuilder builder
                  IO.putBuilder (App.footer Nothing roots)

      liftIO $
        case output of
          Nothing ->
            write "elm.js"

          Just Flags.None ->
            return ()

          Just (Flags.Custom maybeDir fileName) ->
            write =<< Flags.safeCustomPath maybeDir fileName



-- GENERATE REPL MONOLITH


generateReplFile :: Summary.Summary -> Crawl.Graph () -> Repl.Output -> Task.Task FilePath
generateReplFile summary@(Summary.Summary _ project _ _ _) graph output =
  do
      objectGraph <- organize summary graph
      let pkg = Project.getName project
      let roots = Repl.toRoots pkg output
      let builder = Compiler.generate True Compiler.Server objectGraph roots

      liftIO $ IO.put Paths.temp $
        do  IO.putBuilder Repl.header
            IO.putBuilder builder
            IO.putBuilder (Repl.footer pkg output)

      return Paths.temp



-- ORGANIZE


organize :: Summary.Summary -> Crawl.Graph () -> Task.Task Obj.Graph
organize (Summary.Summary root _ _ _ deps) (Crawl.Graph _ locals _ _ _) =
  do  localObjs <- Obj.unions <$> traverse (loadModuleObj root) (Map.keys locals)
      foreignObjs <- Obj.unions <$> traverse loadPackageObj (Map.toList deps)
      return (Obj.union localObjs foreignObjs)


loadModuleObj :: FilePath -> Module.Raw -> Task.Task Obj.Graph
loadModuleObj root name =
  IO.readBinary (Paths.elmo root name)


loadPackageObj :: ( Pkg.Name, (Pkg.Version, deps) ) -> Task.Task Obj.Graph
loadPackageObj ( name, (version,_) ) =
  do  dir <- Task.getPackageCacheDirFor name version
      IO.readBinary (dir </> "objs.dat")

