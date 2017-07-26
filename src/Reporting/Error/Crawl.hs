{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Crawl
  ( Error(..)
  , Problem(..)
  , toDoc
  )
  where


import qualified Data.Text as Text
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Reporting.Error.Help as Help



-- ERROR


data Error
  = RootFileNotFound FilePath
  | RootModuleNameDuplicate Module.Raw [FilePath]
  | RootNameless FilePath
  | DependencyProblems Problem [Problem]


data Problem
  = ModuleNotFound (Maybe Module.Raw) Module.Raw -- TODO suggest other names
  | ModuleAmbiguous (Maybe Module.Raw) Module.Raw [FilePath] [Pkg.Package]
  | BadHeader FilePath Text.Text Compiler.Error
  | ModuleNameMissing FilePath Module.Raw
  | ModuleNameMismatch
      FilePath
      Module.Raw -- expected
      Module.Raw -- actual
  | PortsInPackage Module.Raw
  | EffectsUnexpected Module.Raw



-- TO DOC


toDoc :: Error -> P.Doc
toDoc err =
  case err of
    RootFileNotFound path ->
      Help.makeErrorDoc
        "You want me to compile this file:"
        [ P.indent 4 $ P.dullyellow $ P.text path
        , "I cannot find it though! Is there a typo?"
        ]

    RootModuleNameDuplicate name paths ->
      Help.makeErrorDoc
        "I am trying to compile multiple modules with the same name:"
        [ P.indent 4 $ P.dullyellow $ P.vcat $
            map P.text paths
        , P.fillSep $
            [ "These", "modules", if length paths == 2 then "both" else "all", "claim"
            , "to", "be", "named", P.dullyellow (P.text (Module.nameToString name)) <> "."
            , "Change", "them", "to", "have", "unique", "names", "and", "you"
            , "should", "be", "all", "set!"
            ]
        ]

    RootNameless path ->
      namelessToDoc path "Main"

    DependencyProblems problem otherProblems ->
      case otherProblems of
        [] ->
          problemToDoc problem

        _ ->
          error "TODO handle multiple dependency errors"



-- PROBLEM TO DOC


problemToDoc :: Problem -> P.Doc
problemToDoc problem =
  case problem of
    ModuleNotFound Nothing name ->
      Help.makeErrorDoc
        "Your elm.json says your project has the following module:"
        [ P.indent 4 $ P.dullyellow $ P.text $ Module.nameToString name
        , Help.reflow $
            "I cannot find it though! Is there a typo in the module name? Or\
            \ maybe some source directory is missing or misspelled in elm.json?"
        ]

    ModuleNotFound (Just parent) child ->
      Help.makeErrorDoc
        ( "The " ++ Module.nameToString parent ++ " module wants to import:"
        )
        [ P.indent 4 $ P.dullyellow $ P.text $ Module.nameToString child
        , Help.reflow $
            "I cannot find it though! Try going through these questions:"
        , P.vcat $
            [ "  1. Is there a typo in the module name?"
            , "  2. Is it in a package? Did you `elm install` that package yet?"
            , "  3. Is it a local file? Is it in a source directory listed in your elm.json?"
            ]
        ]

    ModuleAmbiguous maybeParent child paths pkgs ->
      ambiguousToDoc maybeParent child paths pkgs

    BadHeader path source compilerError ->
      Compiler.errorToDoc Compiler.dummyLocalizer path source compilerError

    ModuleNameMissing path name ->
      namelessToDoc path name

    ModuleNameMismatch path expected actual ->
      Help.makeErrorDoc
        ( "The file at " ++ path ++ " has a typo in the module name. It says:"
        )
        [ P.indent 4 $ "module" <+> P.red (P.text (Module.nameToString actual)) <+> "exposing (..)"
        , "Looks like a typo or copy/paste error. Instead it needs to say:"
        , P.indent 4 $ "module" <+> P.green (P.text (Module.nameToString expected)) <+> "exposing (..)"
        , "Make the change and you should be all set!"
        ]

    PortsInPackage name ->
      badTagToDoc name "port" "port-modules" $
        "Packages cannot have any `port` modules."

    EffectsUnexpected name ->
      badTagToDoc name "effect" "effect-modules" $
        "Creating `effect` modules is relatively experimental. There are a\
        \ couple in @elm-lang repos right now, but we have decided to be\
        \ very cautious in expanding its usage."


badTagToDoc :: Module.Raw -> String -> String -> String -> P.Doc
badTagToDoc name tag hintName summary =
  Help.makeErrorDoc summary
    [ P.fillSep $
        [ "Get", "rid", "of", "all", "the"
        , P.red (P.text tag)
        , "stuff", "in"
        , P.dullyellow (P.text (Module.nameToString name))
        , "to", "proceed."
        ]
    , Help.note $
        "You can learn the reasoning behind this design choice at "
        ++ Help.hintLink hintName
    ]



-- HELPERS


namelessToDoc :: FilePath -> Module.Raw -> P.Doc
namelessToDoc path name =
  Help.makeErrorDoc
    ( "The file at " ++ path ++ " must start with a line like this:"
    )
    [ P.indent 4 $ P.dullyellow $ P.text $
        "module " ++ Module.nameToString name ++ " exposing (..)"
    , Help.reflow $
        "Try adding that as the first line of your file!"
    , Help.note $
        "It is best to replace (..) with an explicit list of types and\
        \ functions you want to expose. If you know a value is only used\
        \ WITHIN this module, it is extra easy to refactor. This kind of\
        \ information is great, especially as your project grows!"
    ]


ambiguousToDoc :: Maybe Module.Raw -> Module.Raw -> [FilePath] -> [Pkg.Package] -> P.Doc
ambiguousToDoc maybeParent child paths pkgs =
  let
    summary =
      case maybeParent of
        Just parent ->
          "The " ++ Module.nameToString parent ++ " module wants to import:"

        Nothing ->
          "Your elm.json wants the following module:"

    pkgToString (Pkg.Package pkg vsn) =
      "exposed by " ++ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn
  in
  Help.makeErrorDoc summary $
    [ P.indent 4 $ P.dullyellow $ P.text $ Module.nameToString child
    , Help.reflow $
        "I found multiple module with that name though!"
    , P.indent 4 $ P.dullyellow $ P.vcat $
        map P.text $ paths ++ map pkgToString pkgs
    , Help.reflow $
        if null paths then
          "It looks like the name clash is in your dependencies, which is\
          \ out of your control. Elm does not support this scenario right\
          \ now, but it may be worthwhile. Please open an issue describing\
          \ your scenario so we can gather more usage information!"
        else
          "Which is the right one? Try renaming your modules to have unique names."
    ]
