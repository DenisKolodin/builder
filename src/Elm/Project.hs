{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( module Elm.Project.Internal
  , path
  , unsafeRead
  , write
  , toName, toPkgName, toPkgVersion
  , toSourceDir, toNative
  , matchesCompilerVersion
  , toSolution, toDirectDeps
  , toRoots
  )
  where

import Prelude hiding (read)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import Elm.Project.Internal
import qualified Elm.Project.Constraint as C
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task



-- PATH


path :: FilePath
path =
  "elm.json"



-- READ


unsafeRead :: FilePath -> Task.Task Project
unsafeRead filePath =
  do  byteString <- liftIO (BS.readFile filePath)
      case parse byteString of
        Right project ->
          return project

        Left err ->
          Task.throw (Error.Assets (E.CorruptProject path err))



-- WRITE


write :: FilePath -> Project -> IO ()
write filePath project =
  writeFile filePath (projectToString project)


projectToString :: Project -> String
projectToString _project =
  error "TODO projectToString"



-- VALIDATE


checkOverlap :: Map Name a -> Map Name a -> Either (Set.Set Name) ()
checkOverlap deps tests =
  let
    overlap =
      Map.intersection deps tests
  in
    if Map.null overlap then
      Right ()
    else
      Left (Map.keysSet overlap)



-- REPO


toName :: Project -> Maybe Name
toName project =
  destruct (const Nothing) (Just . toPkgName) project


toPkgName :: PkgInfo -> Name
toPkgName info =
  _pkg_name info


toPkgVersion :: PkgInfo -> Version
toPkgVersion info =
  _pkg_version info



-- EXTRACT INFORMARION


toSourceDir :: Project -> FilePath
toSourceDir project =
  destruct _app_source_dir (const "src") project


toNative :: Project -> Bool
toNative project =
  destruct (const False) _pkg_natives project


destruct :: (AppInfo -> a) -> (PkgInfo -> a) -> Project -> a
destruct appFunc pkgFunc project =
  case project of
    App info ->
      appFunc info

    Pkg info ->
      pkgFunc info


matchesCompilerVersion :: Project -> Bool
matchesCompilerVersion project =
  case project of
    App info ->
      _app_elm_version info == Compiler.version

    Pkg info ->
      C.goodElm (_pkg_elm_version info)


toSolution :: Project -> Map Name Version
toSolution project =
  toSolutionHelp $ destruct _app_deps _pkg_transitive_deps project


toSolutionHelp :: TransitiveDeps -> Map Name Version
toSolutionHelp (TransitiveDeps a b c d) =
  Map.unions [a,b,c,d]


toDirectDeps :: Project -> Set.Set Name
toDirectDeps project =
  toDirectDepsHelp $ destruct _app_deps _pkg_transitive_deps project


toDirectDepsHelp :: TransitiveDeps -> Set.Set Name
toDirectDepsHelp (TransitiveDeps a b _ _) =
  Map.keysSet (Map.union a b)


toRoots :: Project -> [Module.Raw]
toRoots project =
  destruct _app_pages _pkg_exposed project

