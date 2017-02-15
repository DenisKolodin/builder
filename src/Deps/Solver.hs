module Deps.Solver (solve) where

import Control.Monad (foldM, guard, mzero, msum)
import Control.Monad.Logic (LogicT, runLogicT, lift)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import Elm.Package (Name, Version)

import qualified Deps.Explorer as Explorer
import Elm.Project.Constraint (Constraint)
import qualified Elm.Project.Constraint as Con
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- SOLVE


solve :: Constraint -> Map Name Constraint -> Task.Task (Maybe (Map Name Version))
solve elm cons =
  if not (Con.goodElm elm) then
    Task.throw (Error.ElmVersionMismatch elm)

  else
    Explorer.run $
      runLogicT
        (mkSolver (State Map.empty cons))
        (const . return . Just)
        (return Nothing)


type Solver a =
  LogicT Explorer.Explorer a



-- SOLVER


data State =
  State
    { _solution :: Map Name Version
    , _unsolved :: Map Name Constraint
    }


mkSolver :: State -> Solver (Map Name Version)
mkSolver (State solution unsolved) =
  case Map.minViewWithKey unsolved of
    Nothing ->
      return solution

    Just ((name, constraint), otherUnsolved) ->
      do  allVersions <- lift $ Explorer.getVersions name
          let versions =
                reverse $ List.sort $
                  filter (Con.satisfies constraint) allVersions

          let state1 = State solution otherUnsolved
          state2 <- msum (map (addVersion state1 name) versions)
          mkSolver state2


addVersion :: State -> Name -> Version -> Solver State
addVersion (State solution unsolved) name version =
  do  (Explorer.Info elm cons) <-
        lift $ Explorer.getConstraints name version

      guard (Con.goodElm elm)
      newUnsolved <- foldM (addConstraint solution) unsolved (Map.toList cons)
      return (State (Map.insert name version solution) newUnsolved)


addConstraint :: Map Name Version -> Map Name Constraint -> (Name, Constraint) -> Solver (Map Name Constraint)
addConstraint solution unsolved (name, newConstraint) =
  case Map.lookup name solution of
    Just version ->
      if Con.satisfies newConstraint version then
        return unsolved
      else
        mzero

    Nothing ->
      case Map.lookup name unsolved of
        Nothing ->
          return $ Map.insert name newConstraint unsolved

        Just oldConstraint ->
          case Con.intersect oldConstraint newConstraint of
            Nothing ->
              mzero

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint then
                return unsolved
              else
                return (Map.insert name mergedConstraint unsolved)



{-- FAILURE HINTS


incompatibleWithCompiler :: (Name, Constraint) -> Explorer (Maybe Error.Hint)
incompatibleWithCompiler (name, constraint) =
  do  allVersions <- Explorer.getVersions name
      let presentAndFutureVersions =
            filter (\vsn -> Con.check constraint vsn /= LT) allVersions

      compilerConstraints <-
        forM presentAndFutureVersions $ \vsn ->
          do  elmConstraint <- fst <$> Explorer.getConstraints name vsn
              return (vsn, elmConstraint)

      case filter (Con.goodElm . snd) compilerConstraints of
        [] ->
          return $ Just $ Error.IncompatiblePackage name

        compatibleVersions ->
          case filter (Con.satisfies constraint . fst) compilerConstraints of
            [] ->
              return $ Just $ Error.EmptyConstraint name constraint

            pairs ->
              if any (Con.goodElm . snd) pairs then
                return Nothing

              else
                return $ Just $
                  Error.IncompatibleConstraint name constraint $
                    maximum (map fst compatibleVersions)

-}