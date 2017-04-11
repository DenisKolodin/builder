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
import qualified File.Args as Args
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


compile :: Summary -> Args.Args FilePath -> Task.Task ()
compile summary args =
  do  graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write answers
      Output.generate summary graph


compileSource :: Summary -> FilePath -> Text -> Task.Task (Map Module.Raw Compiler.Result)
compileSource summary path source =
  do  graph <- Crawl.crawlFromSource summary path source
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write answers
      -- TODO generate JS in one (big?) bundle
      -- TODO perhaps it can be cut up for elm-lang.org?
      return results
