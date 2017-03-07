{-# OPTIONS_GHC -Wall #-}
module Elm.Project
  ( module Elm.Project.Internal
  , read
  , write

  , getDirectDeps
  , appSolution

  , get
  , getName
  , getSourceDir
  , getNative
  , getEffect
  , getRoots
  )
  where

import Prelude hiding (read)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
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


checkOverlap :: Map Name a -> Map Name a -> Either (Set Name) ()
checkOverlap deps tests =
  let
    overlap =
      Map.intersection deps tests
  in
    if Map.null overlap then
      Right ()
    else
      Left (Map.keysSet overlap)



-- DEPENDENCIES


getDirectDeps :: Project -> Set Name
getDirectDeps project =
  get (Map.keysSet . _app_deps) (Map.keysSet . _pkg_deps) project


appSolution :: AppInfo -> Map Name Version
appSolution info =
  Map.unions
    [ _app_deps info
    , _app_test_deps info
    , _app_trans_deps info
    ]



-- REPO


getName :: Project -> Name
getName project =
  get (\_ -> Pkg.dummyName) _pkg_name project



-- EXTRACT INFORMARION


get :: (AppInfo -> a) -> (PkgInfo -> a) -> Project -> a
get appFunc pkgFunc project =
  case project of
    App info ->
      appFunc info

    Pkg info ->
      pkgFunc info


getSourceDir :: Project -> FilePath
getSourceDir project =
  get _app_source_dir (const "src") project


getNative :: Project -> Bool
getNative project =
  get (const False) _pkg_natives project


getEffect :: Project -> Bool
getEffect project =
  get (const False) _pkg_effects project


getRoots :: Project -> [Module.Raw]
getRoots project =
  get _app_pages _pkg_exposed project

