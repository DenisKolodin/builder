module Deps.Solver.App (add) where

import Control.Monad (filterM, forM, msum)
import Control.Monad.Trans (lift)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Elm.Package (Name, Version)

import Deps.Explorer (Explorer)
import qualified Deps.Explorer as Explorer
import Deps.Solver.Internal (Solver)
import qualified Deps.Solver.Internal as Solver
import qualified Elm.Compiler as Compiler
import Elm.Project (AppInfo(..), TransitiveDeps(..))
import Elm.Project.Constraint (Constraint)
import qualified Elm.Project.Constraint as Con
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- ADD


add :: [Name] -> AppInfo -> Task.Task (Map Name Version)
add names info =
  if _app_elm_version info /= Compiler.version then
    error "TODO"

  else
    let
      (TransitiveDeps deps tests _ _) =
        _app_deps info

      versions =
        Map.union deps tests
    in
      Explorer.run $
        do  maybeSolution <- Solver.run (tryFourWays names versions)
            case maybeSolution of
              Just solution ->
                return solution

              Nothing ->
                do  badNames <- filterM isBadElm (names ++ Map.keys versions)
                    lift $ Task.throw (Error.NoSolution badNames)


tryFourWays :: [Name] -> Map Name Version -> Solver (Map Name Version)
tryFourWays names versions =
  let
    newCons =
      Map.fromList (map (flip (,) Con.anything) names)
  in
    msum $ map (Solver.solve . Map.union newCons) $
      [ Map.map Con.exactly versions
      , Map.map Con.untilNextMinor versions
      , Map.map Con.untilNextMajor versions
      , Map.map (\_ -> Con.anything) versions
      ]



-- FAILURE HINTS


isBadElm :: Name -> Explorer Bool
isBadElm name =
  do  versions <- Explorer.getVersions name

      elmVersions <- forM versions $ \vsn ->
        Explorer._elm <$> Explorer.getConstraints name vsn

      return (not (any Con.goodElm elmVersions))
