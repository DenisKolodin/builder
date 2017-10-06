{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Crawl
  ( Error(..)
  , Problem(..)
  , Origin(..)
  , toReport
  )
  where


import qualified Data.Char as Char
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
  = ModuleNotFound Origin Module.Raw -- TODO suggest other names
  | ModuleAmbiguous Origin Module.Raw [FilePath] [Pkg.Package]
  | BadHeader FilePath Text.Text Compiler.Error
  | ModuleNameReservedForKernel Module.Raw
  | ModuleNameMissing FilePath Module.Raw
  | ModuleNameMismatch
      FilePath
      Module.Raw -- expected
      Module.Raw -- actual
  | PortsInPackage FilePath Module.Raw
  | EffectsUnexpected FilePath Module.Raw


data Origin
  = ElmJson
  | File FilePath
  | Module FilePath Module.Raw



-- TO REPORT


toReport :: Error -> Help.Report
toReport err =
  case err of
    RootFileNotFound path ->
      Help.report "FILE NOT FOUND" Nothing
        "You want me to compile this file:"
        [ P.indent 4 $ P.dullyellow $ P.text path
        , "I cannot find it though! Is there a typo?"
        ]

    RootModuleNameDuplicate name paths ->
      Help.report "DUPLICATE NAMES" Nothing
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
          problemToReport problem

        _ ->
          error "TODO handle multiple dependency errors"



-- PROBLEM TO REPORT


problemToReport :: Problem -> Help.Report
problemToReport problem =
  case problem of
    ModuleNotFound origin name ->
      notFoundToDoc origin name

    ModuleAmbiguous origin child paths pkgs ->
      ambiguousToDoc origin child paths pkgs

    BadHeader path source compilerError ->
      Help.compilerReport $
        Compiler.errorToDoc Compiler.dummyLocalizer path source compilerError

    ModuleNameMissing path name ->
      namelessToDoc path name

    ModuleNameMismatch path expected actual ->
      Help.report "MODULE NAME MISMATCH" (Just path)
        ( "The file at " ++ path ++ " has a typo in the module name. It says:"
        )
        [ P.indent 4 $ P.dullyellow $ "module" <+> P.red (P.text (Module.nameToString actual)) <+> "exposing (..)"
        , "Looks like a typo or copy/paste error. Instead it needs to say:"
        , P.indent 4 $ P.dullyellow $ "module" <+> P.green (P.text (Module.nameToString expected)) <+> "exposing (..)"
        , "Make the change and you should be all set!"
        ]

    PortsInPackage path name ->
      badTagToDoc path name "port" "port-modules" $
        "Packages cannot have any `port` modules."

    EffectsUnexpected path name ->
      badTagToDoc path name "effect" "effect-modules" $
        "Creating `effect` modules is relatively experimental. There are a\
        \ couple in @elm-lang repos right now, but we have decided to be\
        \ very cautious in expanding its usage."


badTagToDoc :: FilePath -> Module.Raw -> String -> String -> String -> Help.Report
badTagToDoc path name tag hintName summary =
  Help.report
    ("UNEXPECTED " ++ map Char.toUpper tag ++ " MODULE")
    (Just path)
    summary
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


namelessToDoc :: FilePath -> Module.Raw -> Help.Report
namelessToDoc path name =
  Help.report "UNNAMED MODULE" (Just path)
    ( "The `" ++ Module.nameToString name
      ++ "` module must start with a line like this:"
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


notFoundToDoc :: Origin -> Module.Raw -> Help.Report
notFoundToDoc origin child =
  case origin of
    ElmJson ->
      Help.report "MODULE NOT FOUND" Nothing
        "Your elm.json says your project has the following module:"
        [ P.indent 4 $ P.dullyellow $ P.text $ Module.nameToString child
        , Help.reflow $
            "I cannot find it though! Is there a typo in the module name? Or\
            \ maybe some source directory is missing or misspelled in elm.json?"
        ]

    File path ->
      Help.report "UNKNOWN IMPORT" (Just path)
        ("The " ++ path ++ " file has a bad import:")
        (notFoundDetails child)

    Module path parent ->
      Help.report "UNKNOWN IMPORT" (Just path)
        ("The " ++ Module.nameToString parent ++ " module has a bad import:")
        (notFoundDetails child)


notFoundDetails :: Module.Raw -> [P.Doc]
notFoundDetails child =
  [ P.indent 4 $ P.dullyellow $ P.text $ "import " ++ Module.nameToString child
  , "I cannot find that module! Is there a typo in the module name?"
  , P.vcat
      [ "Or maybe the module is in a package? Did you `elm install` that package yet?"
      , Help.reflow $
          "Or maybe it is a file you wrote and is sitting in a directory? Is it\
          \ possible that the \"source-directories\" field in elm.json is messed up?"
      ]
  ]


ambiguousToDoc :: Origin -> Module.Raw -> [FilePath] -> [Pkg.Package] -> Help.Report
ambiguousToDoc origin child paths pkgs =
  let
    pkgToString (Pkg.Package pkg vsn) =
      "exposed by " ++ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn

    makeReport maybePath summary yellowString =
      Help.report "AMBIGUOUS IMPORT" maybePath summary
        [ P.indent 4 $ P.dullyellow $ P.text yellowString
        , Help.reflow $
            "I found multiple module with that name though:"
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
  in
    case origin of
      ElmJson ->
        makeReport
          Nothing
          "Your elm.json wants the following module:"
          (Module.nameToString child)

      File path ->
        makeReport
          (Just path)
          ("The file at " ++ path ++ " has an ambiguous import:")
          ("import " ++ Module.nameToString child)

      Module path parent ->
        makeReport
          (Just path)
          ("The " ++ Module.nameToString parent ++ " module has an ambiguous import:")
          ("import " ++ Module.nameToString child)
