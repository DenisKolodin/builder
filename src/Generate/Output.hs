{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Output
  ( generate
  )
  where


import Control.Monad.Trans (liftIO)
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
import qualified File.Crawl as Crawl
import qualified File.Hash as Hash
import qualified Generate.App as App
import qualified Generate.Organize as Organize
import qualified Reporting.Task as Task



-- GENERATE


generate :: Summary.Summary -> Crawl.Graph () -> Module.Raw -> Task.Task ()
generate summary@(Summary.Summary _ project _ _ deps) graph name =
  do
      objectGraph <- Organize.organize summary graph
      let root = Obj.root (Project.getName project) name
      let (natives, builder) = Compiler.generate (Obj.symbolTable Map.empty) objectGraph root

      cacheDir <- Task.getPackageCacheDir
      hash <- liftIO $ Hash.put "temp.js" $
        do  Hash.putBuilder App.header
            mapM_ (Hash.putFile . pathTo cacheDir deps) natives
            Hash.putBuilder builder
            Hash.putBuilder (App.footer Nothing [Module.Canonical Pkg.dummyName name])

      liftIO $ Dir.renameFile "temp.js" (Hash.toString hash ++ ".js")



-- APPEND KERNELS


pathTo :: FilePath -> Summary.DepsGraph -> Module.Canonical -> FilePath
pathTo cacheDir deps (Module.Canonical pkg name) =
  cacheDir
  </> Pkg.toFilePath pkg
  </> Pkg.versionToString (fst (deps ! pkg))
  </> "src"
  </> Module.nameToPath name
  <.> "js"
