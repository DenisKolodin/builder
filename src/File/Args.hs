{-# OPTIONS_GHC -Wall #-}
module File.Args
  ( Args(..)
  , fromPaths
  , fromSummary
  )
  where


import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Elm.Compiler.Module as Module
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Generate.Plan as Plan
import qualified Reporting.Task as Task


data Args roots
  = Pkg [Module.Raw]
  | App Plan.Plan
  | Roots (NonEmpty roots)


fromPaths :: Summary.Summary -> [FilePath] -> Task.Task (Args FilePath)
fromPaths summary paths =
  case paths of
    [] ->
      fromSummary summary

    first : rest ->
      return $ Roots (first :| rest)


fromSummary :: Summary.Summary -> Task.Task (Args a)
fromSummary (Summary.Summary _ project _ _ _) =
  case project of
    Project.Pkg info ->
      return $ Pkg (Project._pkg_exposed info)

    Project.App _ Nothing ->
      Task.throw (error "TODO cannot call elm make with no files and no plan")

    Project.App _ (Just plan) ->
      return $ App plan
