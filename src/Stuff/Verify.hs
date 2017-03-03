{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
module Stuff.Verify
  ( verify
  )
  where


import qualified Data.Map as Map

import qualified Elm.Compiler as Compiler

import qualified Deps.Verify as Verify
import Elm.Project (Project(..), AppInfo(..), PkgInfo(..))
import qualified Elm.Project.Constraint as Con
import qualified File.IO as IO
import qualified Reporting.Task as Task
import qualified Stuff.Deps as Deps
import qualified Stuff.Paths as Path



-- VERIFY


verify :: Project -> Task.Task Deps.Info
verify project =
  do  exists1 <- IO.exists Path.pkgInfo
      exists2 <- IO.exists Path.deps
      if not exists1 || not exists2
        then
          rebuildCache project

        else
          do  cacheProject <- IO.readBinary Path.pkgInfo
              let valid = isValid cacheProject project
              if valid
                then IO.readBinary Path.deps
                else rebuildCache project



-- DOES PROJECT MATCH CACHE?


isValid :: Project -> Project -> Bool
isValid p1 p2 =
  case (p1, p2) of
    (App info1, App info2) ->
      _app_deps info1 == _app_deps info2
      && _app_elm_version info1 == _app_elm_version info2
      && _app_elm_version info1 == Compiler.version

    (Pkg info1, Pkg info2) ->
      _pkg_transitive_deps info1 == _pkg_transitive_deps info2
      && _pkg_dependencies info1 == _pkg_dependencies info2
      && _pkg_test_deps info1 == _pkg_test_deps info2
      && _pkg_elm_version info1 == _pkg_elm_version info2
      && Con.goodElm (_pkg_elm_version info1)

    _ ->
      False



-- ACTUALLY VALIDATE


rebuildCache :: Project -> Task.Task Deps.Info
rebuildCache project =
  do  IO.remove Path.pkgInfo
      IO.remove Path.deps

      depsInfo <- Map.elems <$> Verify.verify project

      IO.writeBinary Path.pkgInfo project
      IO.writeBinary Path.deps depsInfo
      return (Deps.Info depsInfo)

