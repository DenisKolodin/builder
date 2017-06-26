{-# OPTIONS_GHC -Wall #-}
module Elm.Publish (publish) where


import Control.Monad (when)

import qualified Bump
import qualified Deps.Website as Websites
import qualified CommandLine.Helpers as Cmd
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Pkg
import qualified GitHub
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



publish :: Summary -> Task.Task ()
publish (Summary _ project _ _ _) =
  case project of
    Project.App _ _ ->
      Task.throw Error.CannotPublishApp

    Project.Pkg info@(Project.PkgInfo name summary _ version exposed _ _ _ _ _) ->
      do
          Task.report (Progress.PublishStart name version)

          when (null exposed)      $ Task.throw Error.PublishWithoutExposed
          when (isSummary summary) $ Task.throw Error.PublishWithoutSummary

          Bump.validate root info
          commitHash <- verifyTag name version

          Website.register name version commitHash

          Task.report Progress.PublishEnd


isSummary :: Text -> Bool
isSummary summary =
  error "TODO check summary is not default, not empty, etc."


verifyTag :: Pkg.Name -> Pkg.Version -> Task.Task String
verifyTag name version =
  do  publicVersions <- GitHub.getVersionTags name
      if elem version publicVersions
        then return ()
        else Task.throw (Error.MissingTag version)
