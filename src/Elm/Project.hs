{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Generate.Output as Output
import qualified Reporting.Task as Task



-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback



-- COMPILE


compile :: Summary -> Task.Task ()
compile summary =
  do  graph <- Crawl.crawl summary
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write answers
      Output.generate summary graph "Main"


compileSource :: Summary -> FilePath -> Text -> Task.Task (Map Module.Raw Compiler.Result)
compileSource summary path source =
  do  (name, graph) <- Crawl.crawlFromSource summary path source
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write answers
      -- TODO generate JS in one (big?) bundle
      -- TODO perhaps it can be cut up for elm-lang.org?
      return results
