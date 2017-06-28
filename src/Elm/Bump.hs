{-# OPTIONS_GHC -Wall #-}
module Elm.Bump
  ( bump
  , toPossibleBumps
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Deps.Diff as Diff
import qualified Deps.Get as Get
import qualified Elm.Package as Pkg
import qualified Elm.Project as Project
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Error as Error
import qualified Reporting.Progress.Terminal as Terminal
import qualified Reporting.Task as Task



-- BUMP


bump :: Summary.Summary -> Task.Task ()
bump summary@(Summary.Summary root project _ _ _) =
  case project of
    Project.App _ _ ->
      Task.throw Error.CannotBumpApp

    Project.Pkg info@(Project.PkgInfo name _ _ version _ _ _ _ _) ->
      do  packages <- Get.all Get.RequireLatest
          case Map.lookup name packages of
            Nothing ->
              checkNewPackage root info

            Just publishedVersions ->
              let
                bumpableVersions =
                  map (\(old, _, _) -> old) (toPossibleBumps publishedVersions)
              in
                if elem version bumpableVersions then
                  suggestVersion summary info
                else
                  Task.throw $ Error.Unbumpable version $
                    map head (List.group (List.sort bumpableVersions))



-- VALID BUMPS


toPossibleBumps :: [Pkg.Version] -> [(Pkg.Version, Pkg.Version, Diff.Magnitude)]
toPossibleBumps publishedVersions =
  let
    patchPoints =
      Pkg.filterLatest Pkg.majorAndMinor publishedVersions

    minorPoints =
      Pkg.filterLatest Pkg._major publishedVersions

    majorPoint =
      head publishedVersions
  in
    (majorPoint, Pkg.bumpMajor majorPoint, Diff.MAJOR)
    :  map (\v -> (v, Pkg.bumpMinor v, Diff.MINOR)) minorPoints
    ++ map (\v -> (v, Pkg.bumpPatch v, Diff.PATCH)) patchPoints



-- CHECK NEW PACKAGE


checkNewPackage :: FilePath -> Project.PkgInfo -> Task.Task ()
checkNewPackage root info@(Project.PkgInfo _ _ _ version _ _ _ _ _) =
  do  liftIO $ putStrLn Terminal.newPackageOverview
      if version == Pkg.initialVersion
        then
          liftIO $ putStrLn "The version number in elm.json is correct so you are all set!"
        else
          changeVersion root info Pkg.initialVersion $
            "It looks like the version in elm.json has been changed though!\n\
            \Would you like me to change it back to " ++ Pkg.versionToString Pkg.initialVersion ++ "? [Y/n] "



-- SUGGEST VERSION


suggestVersion :: Summary.Summary -> Project.PkgInfo -> Task.Task ()
suggestVersion summary@(Summary.Summary root _ _ _ _) info@(Project.PkgInfo name _ _ version _ _ _ _ _) =
  do  oldDocs <- Get.docs name version
      newDocs <- Task.silently (Project.generateDocs summary)
      let changes = Diff.diff oldDocs newDocs
      let newVersion = Diff.bump changes version
      changeVersion root info newVersion $
        let
          old = Pkg.versionToString version
          new = Pkg.versionToString newVersion
          magnitude = Diff.magnitudeToString (Diff.toMagnitude changes)
        in
          concat
            [ "Based on your new API, this should be a ", magnitude, " change (", old, " => ", new, ")\n"
            , "Bail out of this command and run 'elm-package diff' for a full explanation.\n"
            , "\n"
            , "Should I perform the update (", old, " => ", new, ") in elm.json? [Y/n] "
            ]



-- CHANGE VERSION


changeVersion :: FilePath -> Project.PkgInfo -> Pkg.Version -> String -> Task.Task ()
changeVersion root info targetVersion explanation =
  do  liftIO $ putStr explanation
      approved <- Task.getApproval
      if not approved
        then
          liftIO $ putStrLn "Okay, no changes were made."

        else
          do  liftIO $ Project.write root $ Project.Pkg $
                info { Project._pkg_version = targetVersion }

              liftIO $ putStrLn $
                "Version changed to " ++ Pkg.versionToString targetVersion ++ "!"
