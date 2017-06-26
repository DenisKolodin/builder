{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error
  ( Error(..)
  , Hint(..)
  , toString
  , toStderr
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import Elm.Project.Constraint (Constraint)
import qualified Elm.Project.Constraint as Con
import Deps.Diff (Magnitude)
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

  -- http
  | HttpRequestFailed String String

  -- install
  | NoSolution [Name]

  -- diffs
  | Undiffable
  | VersionInvalid
  | VersionJustChanged
  | MissingTag Version

  -- bumps
  | CannotBumpApp
  | AlreadyPublished Version
  | Unbumpable Version [Version]
  | InvalidBump Version Version
  | BadBump Version Version Magnitude Version Magnitude
  | NotInitialVersion

  -- publish
  | CannotPublishApp
  | PublishWithoutSummary
  | PublishWithoutExposed



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


toDoc :: Error -> P.Doc
toDoc err =
  case err of
    NoElmJson ->
      error "TODO no elm.json file yet. That means it is a new project?"

    Assets assetError ->
      Asset.toDoc assetError

    BadDep name version ->
      error ("TODO BadDep " ++ Pkg.toString name ++ " " ++ Pkg.versionToString version)

    BadCrawlRoot err ->
      error ("TODO crawl root")

    BadCrawl errors ->
      error ("TODO crawl " ++ show (Map.map Crawl.toString errors))

    Cycle names ->
      error ("TODO cycle " ++ show names)

    Compile errors ->
      P.vcat (concatMap Compile.toDocs (Map.elems errors))

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
        [ P.indent 4 $ P.dullyellow $ P.text $ "<" ++ url ++ ">"
        , P.text "Here is the error message I was able to extract:"
        , P.indent 4 $ reflow message
        ]

    NoSolution badPackages ->
      error $
        "TODO the following packages are incompatible with this version of Elm: "
        ++ unwords (map Pkg.toString badPackages)

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

    MissingTag version ->
      let
        vsn =
          Pkg.versionToString version
      in
        Help.makeErrorDoc
          ( "Libraries must be tagged in git, but tag " ++ vsn ++ " was not found."
          )
          [ P.vcat $ map P.text $
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
        [ P.indent 4 $ P.text "elm-package bump"
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
          ++ Pkg.versionToString new ++ ", indicating a " ++ error "TODO magnitude" magnitude
          ++ " change to the public API. This does not match the API diff given by:"
        )
        [ P.indent 4 $ P.text $
            "elm-package diff " ++ Pkg.versionToString old

        , reflow $
          "This command says this is a " ++ error "TODO realMagnitude" realMagnitude
          ++ " change, so the next version should be "
          ++ Pkg.versionToString realNew
          ++ ". Double check everything to make sure you are publishing what you want!"
        , reflow $
            "Also, next time use `elm-package bump` and I'll figure all this out for you!"
        ]

    PublishWithoutSummary ->
      error "TODO bad summary"

    PublishWithoutExposed ->
      error "TODO no exposed modules"


showDependency :: Name -> Constraint -> String
showDependency name constraint =
    show (Pkg.toString name) ++ ": " ++ show (Con.toString constraint)


hintToDoc :: Hint -> P.Doc
hintToDoc hint =
  case hint of
    EmptyConstraint name constraint ->
      stack
        [ reflow $ "Your elm.json has the following dependency:"
        , P.indent 4 $ P.text $ showDependency name constraint
        , reflow $
            "But there are no released versions in that range! I recommend\
            \ removing that constraint by hand and adding it back with:"
        , P.indent 4 $ P.text $ "elm-package install " ++ Pkg.toString name
        ]

    IncompatibleConstraint name constraint viableVersion ->
      stack
        [ reflow $ "Your elm.json has the following dependency:"
        , P.indent 4 $ P.text $ showDependency name constraint
        , reflow $
            "But none of the versions in that range work with Elm "
            ++ Pkg.versionToString Compiler.version ++ ". I recommend removing\
            \ that dependency by hand and adding it back with:"
        , P.indent 4 $
            P.text ("elm-package install " ++ Pkg.toString name)
            <+> P.dullyellow (P.text (Pkg.versionToString viableVersion))
        ]

    IncompatiblePackage name ->
      let
        intro =
          map P.text $ words $
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
              map P.text (words "Maybe the maintainer has not updated it yet.")
      in
        P.fillSep $ intro ++ outro


instead :: String -> [P.Doc]
instead newName =
  map P.text (words "Remove that constraint and use")
  ++ [ P.dullyellow (P.text newName), P.text "instead!" ]


hintToBullet :: Hint -> P.Doc
hintToBullet hint =
  P.dullred (P.text "-->") <+> P.align (hintToDoc hint)
