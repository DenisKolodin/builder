{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( getRoot
  , getRootWithReplFallback
  , compile
  , compileForRepl
  )
  where


import Data.Text (Text)

import qualified Elm.Project.Root as Root
import qualified Elm.Project.Summary as Summary
import Elm.Project.Summary (Summary)
import qualified File.Args as Args
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Generate.Output as Output
import qualified Generate.Repl as Repl
import qualified Reporting.Task as Task



-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback



-- COMPILE


compile :: Summary -> [FilePath] -> Task.Task ()
compile summary paths =
  do  args <- Args.fromPaths summary paths
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write answers
      Output.generate summary graph



-- COMPILE FOR REPL


compileForRepl :: Text -> Maybe String -> Task.Task (Maybe FilePath)
compileForRepl source maybeName =
  do  summary <- getRoot
      graph <- Crawl.crawlFromSource summary source
      (dirty, ifaces) <- Plan.plan summary graph
      let project = Summary._project summary
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write answers
      case maybeName of
        Nothing ->
          return Nothing

        Just name ->
          do  let output = Repl.toOutput results name
              path <- Output.generateReplFile summary graph output
              return (Just path)
