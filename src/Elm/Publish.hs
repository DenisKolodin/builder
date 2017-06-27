{-# OPTIONS_GHC -Wall #-}
module Elm.Publish (publish) where


import Control.Monad (when)
import Control.Monad.Except (catchError)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as Text
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Process

import qualified Deps.Website as Website
import qualified Elm.Bump as Bump
import qualified Elm.Package as Pkg
import qualified Elm.Project as Project
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Args as Args
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- PUBLISH


publish :: Summary.Summary -> Task.Task ()
publish (Summary.Summary root project _ _ _) =
  case project of
    Project.App _ _ ->
      Task.throw Error.CannotPublishApp

    Project.Pkg info@(Project.PkgInfo name summary _ version exposed _ _ _ _) ->
      do
          Task.report (Progress.PublishStart name version)

          when (null exposed)      $ Task.throw Error.PublishWithoutExposed
          when (isSummary summary) $ Task.throw Error.PublishWithoutSummary

          Bump.validate root info
          commitHash <- verifyTag name version
          verifyNoChanges commitHash
          zipHash <- verifyZip root name version

          Website.register name version commitHash zipHash

          Task.report Progress.PublishEnd


isSummary :: Text.Text -> Bool
isSummary summary =
  error "TODO check summary is not default, not empty, etc."


verifyTag :: Pkg.Name -> Pkg.Version -> Task.Task String
verifyTag name version =
  phase (Progress.CheckTag version) $
    Website.githubCommit name version `catchError` \_ ->
      Task.throw (Error.MissingTag version)


verifyNoChanges :: String -> Task.Task ()
verifyNoChanges commitHash =
  phase Progress.CheckChanges $
    do  -- https://stackoverflow.com/questions/3878624/how-do-i-programmatically-determine-if-there-are-uncommited-changes
        exitCode <- liftIO $
          Process.rawSystem "git" [ "diff-index", "--quiet", commitHash, "--" ]

        case exitCode of
          Exit.ExitSuccess ->
            return ()

          Exit.ExitFailure _ ->
            Task.throw (error "TODO local modules do not match")


verifyZip :: FilePath -> Pkg.Name -> Pkg.Version -> Task.Task Website.Sha
verifyZip root name version =
  do  hash <- phase Progress.CheckDownload $
        Website.githubDownload name version

      phase Progress.CheckBuild $ inPrepublishDir $
        do  summary@(Summary.Summary _ project _ _ _) <- Project.getRoot
            args <- Args.fromSummary summary
            graph <- Crawl.crawl summary args
            (dirty, cachedIfaces) <- Plan.plan summary graph
            answers <- Compile.compile project cachedIfaces dirty
            results <- Artifacts.ignore answers
            Artifacts.writeDocs root results

      return hash


inPrepublishDir :: Task.Task a -> Task.Task a
inPrepublishDir task =
  do  runner <- Task.getSilentRunner
      result <- liftIO $ Dir.withCurrentDirectory Path.prepublishDir (runner task)
      either Task.throw return result



-- PHASE REPORTING


phase :: Progress.PublishPhase -> Task.Task a -> Task.Task a
phase publishPhase task =
  do  Task.report $ Progress.PublishProgress publishPhase Nothing

      result <- task `catchError` \err ->
        do  Task.report $ Progress.PublishProgress publishPhase (Just Progress.Bad)
            Task.throw err

      Task.report $ Progress.PublishProgress publishPhase (Just Progress.Good)

      return result
