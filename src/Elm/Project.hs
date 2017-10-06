{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( getRoot
  , getRootWithReplFallback
  , compile
  , compileForRepl
  , generateDocs
  )
  where


import Data.Text (Text)
import System.FilePath ((</>))

import qualified Elm.Docs as Docs
import qualified Elm.Project.Flags as Flags
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
import qualified Stuff.Paths as Path



-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback



-- COMPILE


compile :: Flags.Options -> Summary -> [FilePath] -> Task.Task ()
compile options summary@(Summary.Summary root project _ _ _) paths =
  do  args <- Args.fromPaths summary paths
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan summary graph
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write root answers
      Output.generate options summary graph



-- COMPILE FOR REPL


compileForRepl :: Text -> Maybe String -> Task.Task (Maybe FilePath)
compileForRepl source maybeName =
  do  summary@(Summary.Summary root project _ _ _) <- getRoot
      graph <- Crawl.crawlFromSource summary source
      (dirty, ifaces) <- Plan.plan summary graph
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write root answers
      case maybeName of
        Nothing ->
          return Nothing

        Just name ->
          do  let output = Repl.toOutput results name
              path <- Output.generateReplFile summary graph output
              return (Just path)



-- GENERATE DOCS


generateDocs :: Summary.Summary -> Task.Task Docs.Documentation
generateDocs summary@(Summary.Summary root project _ _ _) =
  do  args <- Args.fromSummary summary
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan summary graph
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.ignore answers
      Artifacts.writeDocs (root </> Path.docs) results
