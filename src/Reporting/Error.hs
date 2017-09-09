{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error
  ( Error(..)
  , Hint(..)
  , toString
  , toStderr
  )
  where


import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import Elm.Project.Constraint (Constraint)
import qualified Elm.Project.Constraint as Con
import qualified Elm.Utils as Utils
import qualified Reporting.Error.Assets as Asset
import qualified Reporting.Error.Bump as Bump
import qualified Reporting.Error.Compile as Compile
import qualified Reporting.Error.Crawl as Crawl
import qualified Reporting.Error.Deps as Deps
import qualified Reporting.Error.Diff as Diff
import qualified Reporting.Error.Help as Help
import qualified Reporting.Error.Http as Http
import qualified Reporting.Error.Publish as Publish



-- ALL POSSIBLE ERRORS


data Error
  = NoElmJson
  | Assets Asset.Error
  | Bump Bump.Error
  | Compile Compile.Error [Compile.Error]
  | Crawl Crawl.Error
  | Cycle [Module.Raw] -- TODO write docs to help with this scenario
  | Deps Deps.Error
  | Diff Diff.Error
  | Publish Publish.Error
  | BadHttp String Http.Error

  -- install
  | NoSolution [Name]



-- BAD CONSTRAINT HINTS


data Hint
  = EmptyConstraint Name Constraint
  | IncompatibleConstraint Name Constraint Version
  | IncompatiblePackage Name



-- RENDERERS


toString :: Error -> String
toString err =
  Help.toString (Help.reportToDoc (toReport err))


toStderr :: Error -> IO ()
toStderr err =
  Help.toStderr (Help.reportToDoc (toReport err))


toReport :: Error -> Help.Report
toReport err =
  case err of
    NoElmJson ->
      error "TODO no elm.json file yet. That means it is a new project?"

    Assets assetError ->
      Asset.toReport assetError

    Bump bumpError ->
      Bump.toReport bumpError

    Compile err errors ->
      Help.compilerReport $ Compile.toDoc err errors

    Crawl error ->
      Crawl.toReport error

    Cycle names ->
      Help.report "IMPORT CYCLE" Nothing
        "Your module imports form a cycle:"
        [ P.indent 4 (Utils.drawCycle names)
        , Help.reflow $
            "Learn more about why this is disallowed and how to break cycles here:"
            ++ Help.hintLink "import-cycles"
        ]

    Deps depsError ->
      Deps.toReport depsError

    Diff commandsError ->
      Diff.toReport commandsError

    Publish publishError ->
      Publish.toReport publishError

    BadHttp url err ->
      Http.toReport url err

    NoSolution badPackages ->
      error $
        "TODO the following packages are incompatible with this version of Elm: "
        ++ unwords (map Pkg.toString badPackages)


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
