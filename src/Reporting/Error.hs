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
import qualified Reporting.Error.Deps as Deps
import qualified Reporting.Error.Help as Help
import qualified Reporting.Error.Http as Http



-- ALL POSSIBLE ERRORS


data Error
  = NoElmJson
  | Assets Asset.Error
  | BadDeps Deps.Error
  | BadCrawlRoot Crawl.Error
  | BadCrawl Crawl.Issues
  | Cycle [Module.Raw] -- TODO write docs to help with this scenario
  | Compile (Map.Map Module.Raw Compile.Error) -- TODO sort compile errors by edit time
  | BadHttp String Http.Error

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
  | PublishWithoutReadme
  | PublishWithShortReadme
  | PublishWithoutLicense



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

    BadDeps depsError ->
      Deps.toDoc depsError

    BadCrawlRoot err ->
      error "TODO problem at the root"

    BadCrawl issues ->
      Crawl.toDoc issues

    Cycle names ->
      error ("TODO cycle " ++ show names)

    Compile errors ->
      P.vcat (concatMap Compile.toDocs (Map.elems errors))

    BadHttp url err ->
      Http.toDoc url err

    NoSolution badPackages ->
      error $
        "TODO the following packages are incompatible with this version of Elm: "
        ++ unwords (map Pkg.toString badPackages)

    Undiffable ->
      Help.makeErrorDoc "This package has not been published, there is nothing to diff against!" []

    VersionInvalid ->
      Help.makeErrorDoc
        "Cannot publish a package with an invalid version. Use `elm bump` to\
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
        [ P.indent 4 $ P.text "elm bump"
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
          [ Help.reflow $
              "Try again after changing the version in elm.json" ++ list
          ]

    InvalidBump statedVersion latestVersion ->
      Help.makeErrorDoc
        ( "Your elm.json says the next version should be "
          ++ Pkg.versionToString statedVersion ++ ", but that is not valid\
          \ based on the previously published versions."
        )
        [ Help.reflow $
            "Generally, you want to put the most recently published version ("
            ++ Pkg.versionToString latestVersion
            ++ " for this package) in your elm.json and run `elm bump` to figure out what should come next."
        ]

    BadBump old new magnitude realNew realMagnitude ->
      Help.makeErrorDoc
        ( "Your elm.json says the next version should be "
          ++ Pkg.versionToString new ++ ", indicating a " ++ error "TODO magnitude" magnitude
          ++ " change to the public API. This does not match the API diff given by:"
        )
        [ P.indent 4 $ P.text $
            "elm diff " ++ Pkg.versionToString old

        , Help.reflow $
          "This command says this is a " ++ error "TODO realMagnitude" realMagnitude
          ++ " change, so the next version should be "
          ++ Pkg.versionToString realNew
          ++ ". Double check everything to make sure you are publishing what you want!"
        , Help.reflow $
            "Also, next time use `elm bump` and I'll figure all this out for you!"
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
      Help.stack
        [ Help.reflow $ "Your elm.json has the following dependency:"
        , P.indent 4 $ P.text $ showDependency name constraint
        , Help.reflow $
            "But there are no released versions in that range! I recommend\
            \ removing that constraint by hand and adding it back with:"
        , P.indent 4 $ P.text $ "elm install " ++ Pkg.toString name
        ]

    IncompatibleConstraint name constraint viableVersion ->
      Help.stack
        [ Help.reflow $ "Your elm.json has the following dependency:"
        , P.indent 4 $ P.text $ showDependency name constraint
        , Help.reflow $
            "But none of the versions in that range work with Elm "
            ++ Pkg.versionToString Compiler.version ++ ". I recommend removing\
            \ that dependency by hand and adding it back with:"
        , P.indent 4 $
            P.text ("elm install " ++ Pkg.toString name)
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
