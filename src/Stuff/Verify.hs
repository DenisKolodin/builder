{-# OPTIONS_GHC -Wall #-}
module Stuff.Verify
  ( verify
  )
  where


import qualified Data.Map as Map
import Data.Map (Map)

import Elm.Package (Name, Version)

import qualified Deps.Verify as Verify
import Elm.Project (Project(..), PkgInfo(..))
import qualified Elm.Project as Project
import qualified Elm.Project.Constraint as Con
import qualified File.IO as IO
import qualified Reporting.Task as Task
import qualified Stuff.Deps as Deps
import qualified Stuff.Paths as Path



-- VERIFY


verify :: Project -> Task.Task Deps.Summary
verify project =
  do  fresh <-
        IO.andM
          [ IO.exists Path.solution
          , IO.exists Path.summary
          , checkSolution project
          ]

      if fresh
        then IO.readBinary Path.summary
        else rebuildCache project


rebuildCache :: Project -> Task.Task Deps.Summary
rebuildCache project =
  do  IO.remove Path.solution
      IO.remove Path.summary

      (solution, summary) <- Verify.verify project

      IO.writeBinary Path.solution solution
      IO.writeBinary Path.summary summary
      return summary



-- CHECK SOLUTION


checkSolution :: Project -> Task.Task Bool
checkSolution project =
  checkSolutionHelp project <$> IO.readBinary Path.solution


checkSolutionHelp :: Project -> Map Name Version -> Bool
checkSolutionHelp project solution =
  case project of
    App info ->
      solution == Project.appSolution info

    Pkg info ->
      allGood (_pkg_test_deps info) solution
      && allGood (_pkg_deps info) solution


allGood :: Map Name Con.Constraint -> Map Name Version -> Bool
allGood cons vsns =
  let
    bools =
      Map.intersectionWith Con.satisfies cons vsns
  in
    Map.size cons == Map.size bools
    && Map.foldr (&&) True bools
