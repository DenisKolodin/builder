module Elm.Project
  ( getRoot
  , getRootWithReplFallback
  , compile
  , compileSource
  )
  where


import Data.Map (Map)
import Data.Text (Text)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module

import qualified Elm.Project.Root as Root
import qualified Elm.Project.Summary as Summary
import Elm.Project.Summary (Summary)
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Reporting.Task as Task



-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback



-- COMPILE


compile :: Summary -> Task.Task (Map Module.Raw Compiler.Result)
compile summary =
  do  graph <- Crawl.crawl summary
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      Compile.compile project ifaces dirty


compileSource :: Summary -> Text -> Task.Task ()
compileSource summary source =
  do  graph <- Crawl.crawl summary
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      Compile.compile project ifaces dirty
      return ()
