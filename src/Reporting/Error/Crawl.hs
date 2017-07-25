{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Crawl
  ( toDoc
  , Error(..)
  , Issues
  , hasIssues
  )
  where


import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Reporting.Error.Help as Help



-- ERRORS


data Error
  = NotFound (Maybe Module.Raw) -- TODO suggest other names
  | Duplicates (Maybe Module.Raw) [FilePath] [Pkg.Package]
  | BadHeader FilePath Text.Text Compiler.Error
  | NoName FilePath
  | BadName FilePath Module.Raw
  | PortsInPackage FilePath
  | EffectsUnexpected FilePath



-- ISSUES


data Issues =
  Issues Module.Raw Error [(Module.Raw, Error)]


hasIssues :: Map.Map Module.Raw Error -> Maybe Issues
hasIssues errors =
  case Map.toList errors of
    [] ->
      Nothing

    (name, err) : others ->
      Just (Issues name err others)



-- TO DOC


toDoc :: Issues -> P.Doc
toDoc (Issues name err otherErrors) =
  case otherErrors of
    [] ->
      errorToDoc name err

    _:_ ->
      error "TODO handle multiple crawl errors"


errorToDoc :: Module.Raw -> Error -> P.Doc
errorToDoc name err =
  case err of
    NotFound Nothing ->
      badImportToDoc Nothing name $
        [ Help.reflow $
            "I cannot find it in your source directories though. Is there a\
            \ typo in the module name? Or in the source directories?"
        ]

    NotFound (Just parent) ->
      badImportToDoc (Just parent) name $
        [ Help.reflow $
            "I cannot find it though! Try going through these questions:"
        , P.vcat $
            [ "  1. Is there a typo in the module name?"
            , "  2. Is it in a package? Did you `elm install` that package yet?"
            , "  3. Is it a local file? Is it in a source directory listed in your elm.json?"
            ]
        ]

    Duplicates maybeParent paths pkgs ->
      badImportToDoc maybeParent name $
        [ Help.reflow $
            "I found multiple module with that name though!"
        , P.indent 4 $ P.dullyellow $ P.vcat $
            map P.text $ paths ++ map pkgToString pkgs
        , Help.reflow $
            if null paths then
              "It looks like the name clash is in your dependencies, which is\
              \ out of your control. Elm does not support this scenario right\
              \ now, but it may be worthwhile. Please open an issue describing\
              \ your scenario if you think this would be an improvement!"
            else
              "Which is the right one? Try renaming your modules to have unique names."
        ]

    BadHeader path source err ->
      Compiler.errorToDoc Compiler.dummyLocalizer path source err

    NoName path ->
      Help.makeErrorDoc
        ( "The file at " ++ path ++ " must start with a line like this:"
        )
        [ P.indent 4 $ P.dullyellow $ P.text $
            "module " ++ Module.nameToString name ++ " exposing (..)"
        , Help.reflow $
            "Ideally you can replace the (..) with an explicit list of types\
            \ and functions you want to expose to other modules. If you know\
            \ a value is only used WITHIN this module, it is extra easy to\
            \ refactor. This kind of information is great, especially as your\
            \ project grows!"
        ]

    BadName path actualName ->
      Help.makeErrorDoc
        ( "The file at " ++ path ++ " has a typo in the module name. It says:"
        )
        [ P.indent 4 $ "module" <+> P.red (P.text (Module.nameToString actualName)) <+> "exposing (..)"
        , "Looks like a typo or copy/paste error. Instead it needs to say:"
        , P.indent 4 $ "module" <+> P.green (P.text (Module.nameToString name)) <+> "exposing (..)"
        , "Make the change and you should be all set!"
        ]

    PortsInPackage _path ->
      badTagToDoc name "port" "port-modules" $
        "Packages cannot have any `port` modules."

    EffectsUnexpected _path ->
      badTagToDoc name "effect" "effect-modules" $
        "Creating `effect` modules is relatively experimental. There are a\
        \ couple in @elm-lang repos right now, but we have decided to be\
        \ very cautious in expanding its usage."


badImportToDoc :: Maybe Module.Raw -> Module.Raw -> [P.Doc] -> P.Doc
badImportToDoc maybeParent name details =
  let
    summary =
      case maybeParent of
        Nothing ->
          "Your elm.json says your project has the following module:"

        Just parent ->
          "The " ++ Module.nameToString parent ++ " module imports the following module:"

    problemModule =
      P.indent 4 $ P.dullyellow $ P.text $ Module.nameToString name
  in
    Help.makeErrorDoc summary (problemModule:details)


pkgToString :: Pkg.Package -> String
pkgToString (Pkg.Package pkg vsn) =
  "exposed by " ++ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn


badTagToDoc :: String -> String -> String -> String -> P.Doc
badTagToDoc name tag hintName summary =
  Help.makeErrorDoc summary
    [ P.fillSep $
        [ "Get", "rid", "of", "all", "the"
        , P.dullyellow (P.text tag)
        , "stuff", "in"
        , P.dullyellow (P.text (Module.nameToString name))
        , "to", "proceed."
        ]
    , Help.reflow $
        "This kind of decision is quite complex, and you can learn more about it here: "
        ++ Help.hintLink hintName
    ]
