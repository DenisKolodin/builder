{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error
  ( Error(..)
  , Hint(..)
  , toString
  , toStderr
  )
  where

import Data.Text (Text)
import qualified Data.List as List
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<+>), align, dullred, dullyellow
  , fillSep, indent, text, vcat
  )

import qualified Elm.Assets as Assets
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg

import qualified Elm.Project.Constraint as C
import qualified Reporting.Error.Assets as AssetsError
import qualified Reporting.Error.Help as Help
import Reporting.Error.Help (reflow, stack)



-- ALL POSSIBLE ERRORS


data Error
  = Assets AssetsError.Error

  -- misc
  | BadElmVersion Pkg.Version Bool C.Constraint
  | SystemCallFailed String
  | HttpRequestFailed String String
  | ZipDownloadFailed Pkg.Name Pkg.Version
  | AddTrickyConstraint Pkg.Name Pkg.Version C.Constraint
  | ConstraintsHaveNoSolution [Hint]
  | BadInstall Pkg.Version

  -- diffs
  | Undiffable
  | VersionInvalid
  | VersionJustChanged
  | BadMetadata [String]
  | MissingTag Pkg.Version

  -- bumps
  | AlreadyPublished Pkg.Version
  | Unbumpable Pkg.Version [Pkg.Version]
  | InvalidBump Pkg.Version Pkg.Version
  | BadBump Pkg.Version Pkg.Version Magnitude Pkg.Version Magnitude

  -- compilation
  | BadCompile FilePath Text [Compiler.Error]


data Magnitude
    = PATCH
    | MINOR
    | MAJOR
    deriving (Eq, Ord, Show)



-- BAD CONSTRAINT HINTS


data Hint
  = EmptyConstraint Pkg.Name C.Constraint
  | IncompatibleConstraint Pkg.Name C.Constraint Pkg.Version
  | IncompatiblePackage Pkg.Name



-- RENDERERS


toString :: Error -> String
toString err =
  Help.toString (toDoc err)


toStderr :: Error -> IO ()
toStderr err =
  Help.toStderr (toDoc err)


toDoc :: Error -> Doc
toDoc err =
  case err of
    Assets assetError ->
      AssetsError.toDoc assetError

    BadCompile file source errors ->
      Help.makeErrorDoc "Bad compile" [text file, text "TODO"]

    BadElmVersion elmVersion isGreater elmConstraint ->
      Help.makeErrorDoc
        ( "You are using Elm " ++ Pkg.versionToString elmVersion
          ++ ", but this project is saying it needs a version in this range: "
          ++ C.toString elmConstraint
        )
        ( map reflow $
            if isGreater then
              [ "This means this package has not been upgraded for the newer version of Elm yet.\
                \ Check out the upgrade docs for guidance on how to get things working again:\
                \ <https://github.com/elm-lang/elm-platform/tree/master/upgrade-docs>"
              ]
            else
              [ "This means the package is written for a newer version of Elm. The best route\
                \ is to just download the new Elm version! <http://elm-lang.org/install>"
              , "If you cannot upgrade for some reason, you can install different versions at\
                \ the same time with npm. I switch between versions by changing my PATH to\
                \ point at certain binaries, but you can do it however you want."
              ]
        )

    SystemCallFailed problem ->
      Help.makeErrorDoc "A system call failed." [ text problem ]

    HttpRequestFailed url message ->
      Help.makeErrorDoc
        ( "The following HTTP request failed. <" ++ url ++ ">"
        )
        [ text message
        ]

    ZipDownloadFailed name version ->
      Help.makeErrorDoc
        ( "Problem when downloading the " ++ Pkg.toString name
          ++ " " ++ Pkg.versionToString version ++ " code."
        )
        []

    ConstraintsHaveNoSolution hints ->
      Help.makeErrorDoc "I cannot find a set of packages that works with your constraints." $
        case hints of
          [] ->
            [ reflow $
                "One way to rebuild your constraints is to clear everything out of\
                \ the \"dependencies\" field of " ++ Assets.projectPath ++ " and add\
                \ them back one at a time with `elm-package install`."
            , reflow $
                "I hope to automate this in the future, but at least there is\
                \ a way to make progress for now!"
            ]

          _ ->
            [ stack (map hintToBullet hints) ]

    AddTrickyConstraint name version constraint ->
      Help.makeErrorDoc
        ( "This change is too tricky for me. Your " ++ Assets.projectPath
          ++ " already lists the following dependency:"
        )
        [ indent 4 $ text $ showDependency name constraint
        , reflow $
            "So I am not sure how to make that include version "
            ++ Pkg.versionToString version
            ++ " as well. Maybe you want one of the following constraints?"
        , indent 4 $ vcat $ map text $
            [ C.toString (C.expand constraint version)
            , C.toString (C.untilNextMajor version)
            ]
        , reflow $
            "Modify " ++ Assets.projectPath ++ " by hand to be exactly what you want."
        ]

    BadInstall version ->
      Help.makeErrorDoc
        ( "You specified a version number, but not a package! Version "
          ++ Pkg.versionToString version ++ " of what?"
        )
        []

    Undiffable ->
      Help.makeErrorDoc "This package has not been published, there is nothing to diff against!" []

    VersionInvalid ->
      Help.makeErrorDoc
        "Cannot publish a package with an invalid version. Use `elm-package bump` to\
        \ figure out what the next version should be, and be sure you commit any\
        \ changes and tag them appropriately."
        []

    VersionJustChanged ->
      Help.makeErrorDoc
        "Cannot publish a package with an invalid version. Be sure you commit any\
        \ necessary changes and tag them appropriately."
        []

    BadMetadata problems ->
      Help.makeErrorDoc
        ( "Some of the fields in " ++ Assets.projectPath ++ " have not been filled in yet:"
        )
        [ vcat (map text problems)
        , text $ "Fill these in and try to publish again!"
        ]

    MissingTag version ->
      let
        vsn =
          Pkg.versionToString version
      in
        Help.makeErrorDoc
          ( "Libraries must be tagged in git, but tag " ++ vsn ++ " was not found."
          )
          [ vcat $ map text $
              [ "These tags make it possible to find this specific version on GitHub."
              , "To tag the most recent commit and push it to GitHub, run this:"
              , ""
              , "    git tag -a " ++ vsn ++ " -m \"release version " ++ vsn ++ "\""
              , "    git push origin " ++ vsn
              ]
          ]

    AlreadyPublished vsn ->
      Help.makeErrorDoc
        ( "Version " ++ Pkg.versionToString vsn ++ " has already been published.\
          \ You cannot publish it again! Run the following command to see what\
          \ the new version should be:"
        )
        [ indent 4 $ text "elm-package bump"
        ]

    Unbumpable vsn versions ->
      let
        list =
          case map Pkg.versionToString versions of
            [v] ->
              " to " ++ v ++ "."

            [v,w] ->
              " to " ++ v ++ " or " ++ w ++ "."

            vsnStrings ->
              " to one of these:  "++ List.intercalate ", " vsnStrings
      in
        Help.makeErrorDoc
          ( "To compute a version bump, I need to start with a version that has\
            \ already been published. Your " ++ Assets.projectPath
            ++ " says I should start with version "
            ++ Pkg.versionToString vsn
            ++ ", but I cannot find that version on <http://package.elm-lang.org>."
          )
          [ reflow $
              "Try again after changing the version in " ++ Assets.projectPath ++ list
          ]

    InvalidBump statedVersion latestVersion ->
      Help.makeErrorDoc
        ( "Your " ++ Assets.projectPath ++ " says the next version should be "
          ++ Pkg.versionToString statedVersion ++ ", but that is not valid\
          \ based on the previously published versions."
        )
        [ reflow $
            "Generally, you want to put the most recently published version ("
            ++ Pkg.versionToString latestVersion ++ " for this package) in your "
            ++ Assets.projectPath
            ++ " and run `elm-package bump` to figure out what should come next."
        ]

    BadBump old new magnitude realNew realMagnitude ->
      Help.makeErrorDoc
        ( "Your " ++ Assets.projectPath ++ " says the next version should be "
          ++ Pkg.versionToString new ++ ", indicating a " ++ show magnitude
          ++ " change to the public API. This does not match the API diff given by:"
        )
        [ indent 4 $ text $
            "elm-package diff " ++ Pkg.versionToString old

        , reflow $
          "This command says this is a " ++ show realMagnitude
          ++ " change, so the next version should be "
          ++ Pkg.versionToString realNew
          ++ ". Double check everything to make sure you are publishing what you want!"
        , reflow $
            "Also, next time use `elm-package bump` and I'll figure all this out for you!"
        ]


showDependency :: Pkg.Name -> C.Constraint -> String
showDependency name constraint =
    show (Pkg.toString name) ++ ": " ++ show (C.toString constraint)


hintToDoc :: Hint -> Doc
hintToDoc hint =
  case hint of
    EmptyConstraint name constraint ->
      stack
        [ reflow $ "Your " ++ Assets.projectPath ++ " has the following dependency:"
        , indent 4 $ text $ showDependency name constraint
        , reflow $
            "But there are no released versions in that range! I recommend\
            \ removing that constraint by hand and adding it back with:"
        , indent 4 $ text $ "elm-package install " ++ Pkg.toString name
        ]

    IncompatibleConstraint name constraint viableVersion ->
      stack
        [ reflow $ "Your " ++ Assets.projectPath ++ " has the following dependency:"
        , indent 4 $ text $ showDependency name constraint
        , reflow $
            "But none of the versions in that range work with Elm "
            ++ Pkg.versionToString Compiler.version ++ ". I recommend removing\
            \ that dependency by hand and adding it back with:"
        , indent 4 $
            text ("elm-package install " ++ Pkg.toString name)
            <+> dullyellow (text (Pkg.versionToString viableVersion))
        ]

    IncompatiblePackage name ->
      let
        intro =
          map text $ words $
            "There are no versions of " ++ Pkg.toString name ++ " that work with Elm "
            ++ Pkg.versionToString Compiler.version ++ "."

        outro =
          case name of
            Pkg.Name "evancz" "elm-svg" ->
              instead "elm-lang/svg"

            Pkg.Name "evancz" "elm-html" ->
              instead "elm-lang/html"

            Pkg.Name "evancz" "virtual-dom" ->
              instead "elm-lang/virtual-dom"

            _ ->
              map text (words "Maybe the maintainer has not updated it yet.")
      in
        fillSep $ intro ++ outro


instead :: String -> [Doc]
instead newName =
  map text (words "Remove that constraint and use")
  ++ [ dullyellow (text newName), text "instead!" ]


hintToBullet :: Hint -> Doc
hintToBullet hint =
  dullred (text "-->") <+> align (hintToDoc hint)
