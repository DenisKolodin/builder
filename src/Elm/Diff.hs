{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Diff
  ( diff
  , toDoc
  )
  where


import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.IO as IO
import Text.PrettyPrint.ANSI.Leijen ((<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Deps.Diff (PackageChanges(..), ModuleChanges(..), Changes(..), Magnitude(..))
import qualified Deps.Diff as Diff
import qualified Deps.Get as Get
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import qualified Elm.Project as Project
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Error.Help as Help
import qualified Reporting.Task as Task



-- DIFF


data Args
  = CodeVsLatest
  | CodeVsExactly Pkg.Version
  | LocalInquiry Pkg.Version Pkg.Version
  | GlobalInquiry Pkg.Name Pkg.Version Pkg.Version


diff :: Args -> Task.Task ()
diff args =
  case args of
    GlobalInquiry name v1 v2 ->
      do  pkgs <- Get.all Get.RequireLatest
          case Map.lookup name pkgs of
            Nothing ->
              error "TODO cannot find that package"

            Just vsns ->
              do  oldDocs <- getDocs name vsns (min v1 v2)
                  newDocs <- getDocs name vsns (max v1 v2)
                  writeDoc (toDoc (Diff.diff oldDocs newDocs))

    LocalInquiry v1 v2 ->
      do  (_, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns (min v1 v2)
          newDocs <- getDocs name vsns (max v1 v2)
          writeDoc (toDoc (Diff.diff oldDocs newDocs))

    CodeVsLatest ->
      do  (summary, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns (maximum vsns)
          newDocs <- Task.silently (Project.generateDocs summary)
          writeDoc (toDoc (Diff.diff oldDocs newDocs))

    CodeVsExactly version ->
      do  (summary, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns version
          newDocs <- Task.silently (Project.generateDocs summary)
          writeDoc (toDoc (Diff.diff oldDocs newDocs))



-- DIFF HELPERS


getDocs :: Pkg.Name -> [Pkg.Version] -> Pkg.Version -> Task.Task Docs.Documentation
getDocs name allVersions version =
  if elem version allVersions then
    Get.docs name version
  else
    error "TODO that version does not exist"


getPackageInfo :: Task.Task (Summary.Summary, Pkg.Name, [Pkg.Version])
getPackageInfo =
  do  summary <- Project.getRoot
      case Summary._project summary of
        Project.App _ _ ->
          Task.throw $ error "TODO cannot diff an app"

        Project.Pkg (Project.PkgInfo name _ _ _ _ _ _ _ _) ->
          do  pkgs <- Get.all Get.RequireLatest
              return ( summary, name, Map.findWithDefault [] name pkgs )



-- WRITE DOC


writeDoc :: P.Doc -> Task.Task ()
writeDoc doc =
  liftIO $ P.displayIO IO.stdout $ P.renderPretty 1 80 doc



-- TO DOC


toDoc :: PackageChanges -> P.Doc
toDoc changes@(PackageChanges added changed removed) =
  let
    header =
      P.text "This is a"
      <+> P.text (Diff.magnitudeToString (Diff.toMagnitude changes))
      <+> P.text "change."

    addedModules =
      if null added then [] else
        [ toHeaderDoc "Added modules" MINOR
        , P.vcat $ map (P.indent 4 . P.text . Module.nameToString) added
        , P.empty
        ]

    removedModules =
      if null removed then [] else
        [ toHeaderDoc "Removed modules" MAJOR
        , P.vcat $ map (P.indent 4 . P.text . Module.nameToString) removed
        , P.empty
        ]
  in
    Help.stack $
      header : addedModules ++ removedModules ++ map changesToDoc (Map.toList changed)


toHeaderDoc :: String -> Magnitude -> P.Doc
toHeaderDoc title magnitude =
  P.dullcyan (P.text "------")
  <+> P.text title
  <+> P.text "-"
  <+> P.text (Diff.magnitudeToString magnitude)
  <+> P.dullcyan (P.text (replicate (64 - length title) '-'))


changesToDoc :: (Text.Text, ModuleChanges) -> P.Doc
changesToDoc (name, changes@(ModuleChanges unions aliases values)) =
  let
    magnitude =
      Diff.moduleChangeMagnitude changes

    (unionAdd, unionChange, unionRemove) =
      changesToDocs unionToDoc unions

    (aliasAdd, aliasChange, aliasRemove) =
      changesToDocs aliasToDoc aliases

    (valueAdd, valueChange, valueRemove) =
      changesToDocs valueToDoc values
  in
    P.vcat
      [ toHeaderDoc ("Changes to module " ++ Text.unpack name) magnitude
      , changesToDocHelp "Added" unionAdd aliasAdd valueAdd
      , changesToDocHelp "Removed" unionRemove aliasRemove valueRemove
      , changesToDocHelp "Changed" unionChange aliasChange valueChange
      ]


changesToDocs :: (k -> v -> P.Doc) -> Changes k v -> ([P.Doc], [P.Doc], [P.Doc])
changesToDocs entryToDoc (Changes added changed removed) =
  let
    indented (name, value) =
      P.text "        " <> entryToDoc name value

    diffed (name, (oldValue, newValue)) =
      P.vcat
        [ P.text "      - " <> entryToDoc name oldValue
        , P.text "      + " <> entryToDoc name newValue
        , P.text ""
        ]
  in
    ( map indented (Map.toList added)
    , map diffed   (Map.toList changed)
    , map indented (Map.toList removed)
    )


changesToDocHelp :: String -> [P.Doc] -> [P.Doc] -> [P.Doc] -> P.Doc
changesToDocHelp categoryName unions aliases values =
  if null unions && null aliases && null values then
    ""

  else
    P.vcat $
      [ P.text ""
      , P.text ""
      , P.text ("    " ++ categoryName ++ ":")
      ]
      ++ unions
      ++ aliases
      ++ values


unionToDoc :: Text.Text -> Docs.Union -> P.Doc
unionToDoc name (Docs.Union tvars ctors) =
  let
    setup =
      P.text "type" <+> text name <+> P.hsep (map text tvars)

    separators =
      map P.text ("=" : repeat "|")

    ctorDoc (ctor, tipes) =
      typeDoc (Type.App (Type.Type ctor) tipes)
  in
    P.hang 4 (P.sep (setup : zipWith (<+>) separators (map ctorDoc ctors)))


aliasToDoc :: Text.Text -> Docs.Alias -> P.Doc
aliasToDoc name (Docs.Alias tvars tipe) =
  let
    typeAlias =
      P.text "type" <+> P.text "alias"

    declaration =
      typeAlias <+> text name <+> P.hsep (map text tvars) <+> P.equals
  in
    P.hang 4 (P.sep [ declaration, typeDoc tipe ])


valueToDoc :: Text.Text -> Docs.Value Type.Type -> P.Doc
valueToDoc name value =
  case value of
    Docs.Value tipe ->
      text name <+> P.colon <+> typeDoc tipe

    Docs.Infix tipe assoc prec ->
      P.text "(" <> text name <> P.text ")" <+> P.colon <+> typeDoc tipe <> infixExtras assoc prec


infixExtras :: Docs.Assoc -> Int -> P.Doc
infixExtras associativity precedence =
  P.text "    |" <> text (Docs.assocToText associativity) <> P.text "-" <> P.int precedence <> P.text "|"


typeDoc :: Type.Type -> P.Doc
typeDoc tipe =
  P.text (Type.toString Type.OneLine tipe)


text :: Text.Text -> P.Doc
text txt =
  P.text (Text.unpack txt)
