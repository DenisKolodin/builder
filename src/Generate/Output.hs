{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Output
  ( generate
  )
  where


import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import qualified System.IO as IO

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Crawl as Crawl
import qualified File.Hash as Hash
import qualified Generate.App as App
import qualified Generate.Organize as Organize
import qualified Reporting.Task as Task



-- GENERATE


generate :: Summary.Summary -> Crawl.Graph () -> Module.Raw -> Task.Task ()
generate summary@(Summary.Summary _ project _ _ deps) graph name =
  do  cacheDir <- Task.getPackageCacheDir

      objectGraph <- Organize.organize summary graph
      let root = Obj.root (Project.getName project) name
      let (natives, builder) = Compiler.generate (Obj.symbolTable Map.empty) objectGraph root

      hash <- liftIO $ IO.withBinaryFile "temp.js" IO.WriteMode $ \handle ->
        do  state1 <- Hash.putBuilder handle Hash.starter App.header
            let append = appendKernel handle cacheDir deps
            state2 <- foldM append state1 natives
            state3 <- Hash.putBuilder handle state2 builder
            Hash.putBuilder handle state3 (App.footer Nothing [Module.Canonical Pkg.dummyName name])

      liftIO $ Dir.renameFile "temp.js" (Hash.toString hash ++ ".js")



-- APPEND KERNELS


appendKernel :: IO.Handle -> FilePath -> Summary.DepsGraph -> Hash.State -> Module.Canonical -> IO Hash.State
appendKernel handle cacheDir deps state name =
  Hash.append handle (pathTo cacheDir deps name) state


pathTo :: FilePath -> Summary.DepsGraph -> Module.Canonical -> FilePath
pathTo cacheDir deps (Module.Canonical pkg name) =
  cacheDir
  </> Pkg.toFilePath pkg
  </> Pkg.versionToString (fst (deps ! pkg))
  </> "src"
  </> Module.nameToPath name
  <.> "js"
