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
import qualified Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<+>), align, dullred, dullyellow
  , fillSep, indent, red, text, vcat
  )

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import Elm.Project.Constraint (Constraint)
import qualified Elm.Project.Constraint as Con
import qualified Reporting.Error.Assets as Asset
import qualified Reporting.Error.Compile as Compile
import qualified Reporting.Error.Crawl as Crawl
import qualified Reporting.Error.Help as Help
import Reporting.Error.Help (reflow, stack)



-- ALL POSSIBLE ERRORS


data Error
  = NoElmJson
  | Assets Asset.Error
  | BadDep Name Version
  | BadCrawlRoot Crawl.Error
  | BadCrawl (Map.Map Module.Raw Crawl.Error)
  | Cycle [Module.Raw] -- TODO write docs to help with this scenario
  | Compile (Map.Map Module.Raw Compile.Error) -- TODO sort compile errors by edit time

  -- verify
  | AppBadElm Version
  | AppBadDeps
  | PkgBadElm Constraint
  | PkgBadDeps

  -- misc
  | HttpRequestFailed String String
  | ZipDownloadFailed Name Version
  | AddTrickyConstraint Name Version Constraint
  | BadInstall Version

  -- diffs
  | Undiffable
  | VersionInvalid
  | VersionJustChanged
  | BadMetadata [String]
  | MissingTag Version

  -- bumps
  | AlreadyPublished Version
  | Unbumpable Version [Version]
  | InvalidBump Version Version
  | BadBump Version Version Magnitude Version Magnitude


data Magnitude
    = PATCH
    | MINOR
    | MAJOR
    deriving (Eq, Ord, Show)



-- BAD CONSTRAINT HINTS


data Hint
  = EmptyConstraint Name Constraint
  | IncompatibleConstraint Name Constraint Version
  | IncompatiblePackage Name



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
      Asset.toDoc assetError

    BadDep name version ->
      error ("TODO " ++ Pkg.toString name ++ " " ++ Pkg.versionToString version)

    BadCrawlRoot err ->
      error ("TODO crawl root")

    BadCrawl errors ->
      error ("TODO crawl " ++ show (Map.map Crawl.toString errors))

    Cycle names ->
      error ("TODO cycle " ++ show names)

    Compile errors ->
      error ("TODO compile " ++ show (map Compile.toDocs (Map.elems errors)))

    AppBadElm version ->
      error ("TODO AppBadElm - " ++ Pkg.versionToString version)

    AppBadDeps ->
      error "TODO AppBadDeps"

    PkgBadElm constraint ->
      error ("TODO PkgBadElm " ++ Con.toString constraint)

    PkgBadDeps ->
      error "TODO PkgBadDeps"

    HttpRequestFailed url message ->
      Help.makeErrorDoc
        ( "The following HTTP request failed:"
        )
        [ indent 4 $ dullyellow $ text $ "<" ++ url ++ ">"
        , text "Here is the error message I was able to extract:"
        , indent 4 $ reflow message
        ]

    ZipDownloadFailed name version ->
      Help.makeErrorDoc
        ( "Problem when downloading the " ++ Pkg.toString name
          ++ " " ++ Pkg.versionToString version ++ " code."
        )
        []

    AddTrickyConstraint name version constraint ->
      Help.makeErrorDoc
        ( "This change is too tricky for me. Your elm.json already lists the following dependency:"
        )
        [ indent 4 $ text $ showDependency name constraint
        , reflow $
            "So I am not sure how to make that include version "
            ++ Pkg.versionToString version
            ++ " as well. Maybe you want one of the following constraints?"
        , indent 4 $ vcat $ map text $
            [ Con.toString (Con.expand constraint version)
            , Con.toString (Con.untilNextMajor version)
            ]
        , reflow $
            "Modify elm.json by hand to be exactly what you want."
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
        ( "Some of the fields in elm.json have not been filled in yet:"
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
            \ already been published. Your elm.json says I should start with version "
            ++ Pkg.versionToString vsn
            ++ ", but I cannot find that version on <http://package.elm-lang.org>."
          )
          [ reflow $
              "Try again after changing the version in elm.json" ++ list
          ]

    InvalidBump statedVersion latestVersion ->
      Help.makeErrorDoc
        ( "Your elm.json says the next version should be "
          ++ Pkg.versionToString statedVersion ++ ", but that is not valid\
          \ based on the previously published versions."
        )
        [ reflow $
            "Generally, you want to put the most recently published version ("
            ++ Pkg.versionToString latestVersion
            ++ " for this package) in your elm.json and run `elm-package bump` to figure out what should come next."
        ]

    BadBump old new magnitude realNew realMagnitude ->
      Help.makeErrorDoc
        ( "Your elm.json says the next version should be "
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


showDependency :: Name -> Constraint -> String
showDependency name constraint =
    show (Pkg.toString name) ++ ": " ++ show (Con.toString constraint)


hintToDoc :: Hint -> Doc
hintToDoc hint =
  case hint of
    EmptyConstraint name constraint ->
      stack
        [ reflow $ "Your elm.json has the following dependency:"
        , indent 4 $ text $ showDependency name constraint
        , reflow $
            "But there are no released versions in that range! I recommend\
            \ removing that constraint by hand and adding it back with:"
        , indent 4 $ text $ "elm-package install " ++ Pkg.toString name
        ]

    IncompatibleConstraint name constraint viableVersion ->
      stack
        [ reflow $ "Your elm.json has the following dependency:"
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
