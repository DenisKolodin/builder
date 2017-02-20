{-# OPTIONS_GHC -Wall #-}
module Elm.Project
  ( module Elm.Project.Internal
  , read
  , write

  , getTransDeps
  , toSolution
  , isSameSolution

  , toName
  , toSourceDir, toNative
  , toRoots
  )
  where

import Prelude hiding (read)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)

import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import Elm.Project.Internal
import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task



-- READ


read :: FilePath -> Task.Task Project
read filePath =
  do  bits <- liftIO (BS.readFile filePath)
      case parse bits of
        Left err ->
          Task.throw (Error.Assets (E.BadElmJson filePath err))

        Right project ->
          return project




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



-- TRANSITIVE DEPS


getTransDeps :: Project -> TransitiveDeps
getTransDeps project =
  destruct _app_deps _pkg_transitive_deps project


toSolution :: TransitiveDeps -> Map Name Version
toSolution (TransitiveDeps a b c d) =
  Map.unions [a,b,c,d]


isSameSolution :: Map Name Version -> TransitiveDeps -> Bool
isSameSolution solution (TransitiveDeps a b c d) =
  solution == Map.unions [ a, b, c, d ]



-- REPO


toName :: Project -> Maybe Name
toName project =
  destruct (const Nothing) (Just . _pkg_name) project



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


toRoots :: Project -> [Module.Raw]
toRoots project =
  destruct _app_pages _pkg_exposed project

