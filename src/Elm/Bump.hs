{-# OPTIONS_GHC -Wall #-}
module Elm.Bump
  ( bump
  , validate
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Deps.Diff as Diff
import qualified Deps.Get as Get
import qualified Elm.Package as Pkg
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- BUMP


bump :: Summary.Summary -> Task.Task ()
bump (Summary.Summary root project _ _ _) =
  case project of
    Project.App _ _ ->
      Task.throw Error.CannotBumpApp

    Project.Pkg info@(Project.PkgInfo name _ _ version _ _ _ _ _) ->
      do  packages <- Get.all Get.RequireLatest
          case Map.lookup name packages of
            Nothing ->
              bumpNewPackage root info

            Just publishedVersions ->
              let
                bumpableVersions =
                  map (\(old, _, _) -> old) (validBumps publishedVersions)
              in
                if elem version bumpableVersions then
                  suggestVersion root info
                else
                  Task.throw $ Error.Unbumpable version $
                    map head (List.group (List.sort bumpableVersions))



-- VALID BUMPS


validBumps :: [Pkg.Version] -> [(Pkg.Version, Pkg.Version, Diff.Magnitude)]
validBumps publishedVersions =
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



-- SUGGEST VERSION


suggestVersion :: FilePath -> Project.PkgInfo -> Task.Task ()
suggestVersion root info@(Project.PkgInfo name _ _ version _ _ _ _ _) =
  do  oldDocs <- Get.docs name version
      newDocs <- error "TODO Compile.toDocs"
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



-- VALIDATE


validate :: FilePath -> Project.PkgInfo -> Task.Task ()
validate root info@(Project.PkgInfo name _ _ version _ _ _ _ _) =
  do  allPackages <- Get.all Get.RequireLatest
      case Map.lookup name allPackages of
        Just publishedVersions ->
          validateNewVersion name version publishedVersions

        Nothing ->
          validateNewPackage root info


validateNewVersion :: Pkg.Name -> Pkg.Version -> [Pkg.Version] -> Task.Task ()
validateNewVersion name statedVersion publishedVersions =
  case List.find (\(_ ,new, _) -> statedVersion == new) (validBumps publishedVersions) of
    Nothing ->
      if elem statedVersion publishedVersions then
        Task.throw $ Error.AlreadyPublished statedVersion

      else
        Task.throw $ Error.InvalidBump statedVersion (last publishedVersions)

    Just (old, new, magnitude) ->
      do  oldDocs <- Get.docs name old
          newDocs <- error "TODO Compile.toDocs"
          let changes = Diff.diff oldDocs newDocs
          let realNew = Diff.bump changes old
          if new == realNew
            then
              do  let oldVsn = Pkg.versionToString old
                  let newVsn = Pkg.versionToString new
                  let mag = Diff.magnitudeToString magnitude
                  liftIO $ putStrLn $
                    "Version number " ++ newVsn ++ " verified (" ++ mag
                    ++ " change, " ++ oldVsn ++ " => " ++ newVsn ++ ")"

            else
              Task.throw $ Error.BadBump old new magnitude realNew $
                Diff.toMagnitude changes



-- CHECK NEW PACKAGE


validateNewPackage :: FilePath -> Project.PkgInfo -> Task.Task ()
validateNewPackage root info@(Project.PkgInfo _ _ _ version _ _ _ _ _) =
  do  liftIO $ putStrLn newPackageOverview
      if version == Pkg.initialVersion
        then
          liftIO $ putStrLn "The version number in elm.json is correct!"
        else
          Task.throw Error.NotInitialVersion


bumpNewPackage :: FilePath -> Project.PkgInfo -> Task.Task ()
bumpNewPackage root info@(Project.PkgInfo _ _ _ version _ _ _ _ _) =
  do  liftIO $ putStrLn newPackageOverview
      if version == Pkg.initialVersion
        then
          liftIO $ putStrLn "The version number in elm.json is correct so you are all set!"
        else
          changeVersion root info Pkg.initialVersion $
            "It looks like the version in elm.json has been changed though!\n\
            \Would you like me to change it back to " ++ Pkg.versionToString Pkg.initialVersion ++ "? [Y/n] "


newPackageOverview :: String
newPackageOverview =
  unlines
    [ "This package has never been published before. Here's how things work:"
    , ""
    , "  - Versions all have exactly three parts: MAJOR.MINOR.PATCH"
    , ""
    , "  - All packages start with initial version " ++ Pkg.versionToString Pkg.initialVersion
    , ""
    , "  - Versions are incremented based on how the API changes:"
    , "      PATCH - the API is the same, no risk of breaking code"
    , "      MINOR - values have been added, existing values are unchanged"
    , "      MAJOR - existing values have been changed or removed"
    , ""
    , "  - I will bump versions for you, automatically enforcing these rules"
    , ""
    ]
