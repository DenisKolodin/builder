{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Diff
  ( diff
  , PackageChanges
  , toString
  , bump
  )
  where


import Control.Monad (zipWithM)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as P

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg



-- CHANGES


data PackageChanges =
  PackageChanges
    { _modules_added :: [Module.Raw]
    , _modules_changed :: Map.Map Module.Raw ModuleChanges
    , _modules_removed :: [Module.Raw]
    }


data ModuleChanges =
  ModuleChanges
    { _unions :: Changes Text Docs.Union
    , _aliases :: Changes Text Docs.Alias
    , _values :: Changes Text (Docs.Value Type.Type)
    }


data Changes k v =
  Changes
    { _added :: Map.Map k v
    , _changed :: Map.Map k (v,v)
    , _removed :: Map.Map k v
    }


getChanges :: (Ord k) => (v -> v -> Bool) -> Map.Map k v -> Map.Map k v -> Changes k v
getChanges isEquivalent old new =
  let
    overlap = Map.intersectionWith (,) old new
    changed = Map.filter (not . uncurry isEquivalent) overlap
  in
    Changes (Map.difference new old) changed (Map.difference old new)



-- DIFF


diff :: Docs.Documentation -> Docs.Documentation -> PackageChanges
diff oldDocs newDocs =
  let
    filterOutPatches chngs =
      Map.filter (\chng -> moduleChangeMagnitude chng /= PATCH) chngs

    (Changes added changed removed) =
      getChanges (\_ _ -> False) oldDocs newDocs
  in
    PackageChanges
      (Map.keys added)
      (filterOutPatches (Map.map diffModule changed))
      (Map.keys removed)



diffModule :: (Docs.Checked, Docs.Checked) -> ModuleChanges
diffModule (Docs.Docs _ us1 as1 vs1, Docs.Docs _ us2 as2 vs2) =
  let
    getEntryChanges isEquiv es1 es2 =
      getChanges isEquiv (Map.map Docs._details es1) (Map.map Docs._details es2)
  in
    ModuleChanges
      (getEntryChanges isEquivalentUnion us1 us2)
      (getEntryChanges isEquivalentAlias as1 as2)
      (getEntryChanges isEquivalentValue vs1 vs2)



-- EQUIVALENCE


isEquivalentUnion :: Docs.Union -> Docs.Union -> Bool
isEquivalentUnion (Docs.Union oldVars oldCtors) (Docs.Union newVars newCtors) =
    length oldCtors == length newCtors
    && and (zipWith (==) (map fst oldCtors) (map fst newCtors))
    && and (Map.elems (Map.intersectionWith equiv (Map.fromList oldCtors) (Map.fromList newCtors)))
  where
    equiv :: [Type.Type] -> [Type.Type] -> Bool
    equiv oldTypes newTypes =
      let
        allEquivalent =
          zipWith
            isEquivalentAlias
            (map (Docs.Alias oldVars) oldTypes)
            (map (Docs.Alias newVars) newTypes)
      in
        length oldTypes == length newTypes
        && and allEquivalent


isEquivalentAlias :: Docs.Alias -> Docs.Alias -> Bool
isEquivalentAlias (Docs.Alias oldVars oldType) (Docs.Alias newVars newType) =
  case diffType oldType newType of
    Nothing ->
      False

    Just renamings ->
      length oldVars == length newVars
      && isEquivalentRenaming (zip oldVars newVars ++ renamings)


isEquivalentValue :: Docs.Value Type.Type -> Docs.Value Type.Type -> Bool
isEquivalentValue oldValue newValue =
  case (oldValue, newValue) of
    (Docs.Value oldType, Docs.Value newType) ->
      isEquivalentAlias (Docs.Alias [] oldType) (Docs.Alias [] newType)

    (Docs.Infix oldType oldAssoc oldPrec, Docs.Infix newType newAssoc newPrec) ->
      isEquivalentAlias (Docs.Alias [] oldType) (Docs.Alias [] newType)
      && oldAssoc == newAssoc
      && oldPrec == newPrec

    (_, _) ->
      False



-- DIFF TYPES


diffType :: Type.Type -> Type.Type -> Maybe [(Text,Text)]
diffType oldType newType =
  case (oldType, newType) of
    (Type.Var oldName, Type.Var newName) ->
      Just [(oldName, newName)]

    (Type.Type oldName, Type.Type newName) ->
      -- TODO handle old names with no module prefixes
      if oldName == newName then
        Just []
      else
        Nothing

    (Type.Lambda a b, Type.Lambda a' b') ->
      (++)
        <$> diffType a a'
        <*> diffType b b'

    (Type.App t ts, Type.App t' ts') ->
      if length ts /= length ts' then
        Nothing
      else
        (++)
          <$> diffType t t'
          <*> (concat <$> zipWithM diffType ts ts')

    (Type.Record fields maybeExt, Type.Record fields' maybeExt') ->
      case (maybeExt, maybeExt') of
        (Nothing, Just _) ->
          Nothing

        (Just _, Nothing) ->
          Nothing

        (Nothing, Nothing) ->
          diffFields fields fields'

        (Just ext, Just ext') ->
          (++)
            <$> diffType ext ext'
            <*> diffFields fields fields'

    (_, _) ->
      Nothing


diffFields :: [(Text, Type.Type)] -> [(Text, Type.Type)] -> Maybe [(Text,Text)]
diffFields oldRawFields newRawFields =
  let
    sort = List.sortBy (compare `on` fst)
    oldFields = sort oldRawFields
    newFields = sort newRawFields
  in
    if length oldRawFields /= length newRawFields then
      Nothing

    else if or (zipWith ((/=) `on` fst) oldFields newFields) then
      Nothing

    else
      concat <$> zipWithM (diffType `on` snd) oldFields newFields



-- TYPE VARIABLES


isEquivalentRenaming :: [(Text,Text)] -> Bool
isEquivalentRenaming varPairs =
  let
    renamings =
      Map.toList (foldr insert Map.empty varPairs)

    insert (old,new) dict =
      Map.insertWith (++) old [new] dict

    verify (old, news) =
      case news of
        [] ->
          Nothing

        new : rest ->
          if all (new ==) rest then
            Just (old, new)
          else
            Nothing

    allUnique list =
      length list == Set.size (Set.fromList list)
  in
    case mapM verify renamings of
      Nothing ->
        False

      Just verifiedRenamings ->
        all compatableVars verifiedRenamings
        &&
        allUnique (map snd verifiedRenamings)


compatableVars :: (Text, Text) -> Bool
compatableVars (old, new) =
  case (categorizeVar old, categorizeVar new) of
    (CompAppend, CompAppend) -> True
    (Comparable, Comparable) -> True
    (Appendable, Appendable) -> True
    (Number    , Number    ) -> True

    (Comparable, CompAppend) -> True
    (Appendable, CompAppend) -> True
    (Number    , CompAppend) -> True
    (Number    , Comparable) -> True

    (_, Var) -> True

    (_, _) -> False


data TypeVarCategory
  = CompAppend
  | Comparable
  | Appendable
  | Number
  | Var


categorizeVar :: Text -> TypeVarCategory
categorizeVar name
  | Text.isPrefixOf "compappend" name = CompAppend
  | Text.isPrefixOf "comparable" name = Comparable
  | Text.isPrefixOf "appendable" name = Appendable
  | Text.isPrefixOf "number"     name = Number
  | otherwise                         = Var



-- MAGNITUDE


data Magnitude
  = PATCH
  | MINOR
  | MAJOR
  deriving (Eq, Ord, Show)


bump :: PackageChanges -> Pkg.Version -> Pkg.Version
bump changes version =
  case packageChangeMagnitude changes of
    PATCH ->
      Pkg.bumpPatch version

    MINOR ->
      Pkg.bumpMinor version

    MAJOR ->
      Pkg.bumpMajor version


packageChangeMagnitude :: PackageChanges -> Magnitude
packageChangeMagnitude (PackageChanges added changed removed) =
  let
    addMag = if null added then PATCH else MINOR
    removeMag = if null removed then PATCH else MAJOR
    changeMags = map moduleChangeMagnitude (Map.elems changed)
  in
    maximum (addMag : removeMag : changeMags)


moduleChangeMagnitude :: ModuleChanges -> Magnitude
moduleChangeMagnitude (ModuleChanges unions aliases values) =
  maximum
    [ changeMagnitude unions
    , changeMagnitude aliases
    , changeMagnitude values
    ]


changeMagnitude :: Changes k v -> Magnitude
changeMagnitude (Changes added changed removed) =
  if Map.size removed > 0 || Map.size changed > 0 then
    MAJOR

  else if Map.size added > 0 then
    MINOR

  else
    PATCH



-- TO STRING


toString :: PackageChanges -> String
toString changes@(PackageChanges added changed removed) =
  concat
    [ "This is a " ++ show (packageChangeMagnitude changes) ++ " change.\n\n"
    ,
      if null added then
        ""
      else
        header "Added modules - MINOR" ++ "\n"
        ++ concatMap ((++) "\n    " . Module.nameToString) added
        ++ "\n\n\n"
    ,
      if null removed then
        ""
      else
        header "Removed modules - MAJOR" ++ "\n"
        ++ concatMap ((++) "\n    " . Module.nameToString) removed
        ++ "\n\n\n"
    ,
      concatMap changesToString (Map.toList changed)
    ]


header :: String -> String
header title =
  let
    len =
      length title
  in
    "------ " ++ title ++ " " ++ replicate (80 - len - 8) '-'


changesToString :: (Text, ModuleChanges) -> String
changesToString (name, changes@(ModuleChanges unions aliases values)) =
  let
    magnitude =
      moduleChangeMagnitude changes

    (unionAdd, unionChange, unionRemove) =
      changesToDocs unionToDoc unions

    (aliasAdd, aliasChange, aliasRemove) =
      changesToDocs aliasToDoc aliases

    (valueAdd, valueChange, valueRemove) =
      changesToDocs valueToDoc values
  in
    header ("Changes to module " ++ Text.unpack name ++ " - " ++ show magnitude)
    ++ changesToStringHelp "Added" unionAdd aliasAdd valueAdd
    ++ changesToStringHelp "Removed" unionRemove aliasRemove valueRemove
    ++ changesToStringHelp "Changed" unionChange aliasChange valueChange
    ++ "\n\n\n"


changesToDocs :: (k -> v -> P.Doc) -> Changes k v -> ([P.Doc], [P.Doc], [P.Doc])
changesToDocs toDoc (Changes added changed removed) =
  let
    indented (name, value) =
      P.text "        " <> toDoc name value

    diffed (name, (oldValue, newValue)) =
      P.vcat
        [ P.text "      - " <> toDoc name oldValue
        , P.text "      + " <> toDoc name newValue
        , P.text ""
        ]
  in
    ( map indented (Map.toList added)
    , map diffed   (Map.toList changed)
    , map indented (Map.toList removed)
    )


changesToStringHelp :: String -> [P.Doc] -> [P.Doc] -> [P.Doc] -> String
changesToStringHelp categoryName unions aliases values =
  if null unions && null aliases && null values then
    ""

  else
    P.renderStyle (P.Style P.PageMode 80 1.0) $ P.vcat $
      [ P.text ""
      , P.text ""
      , P.text ("    " ++ categoryName ++ ":")
      ]
      ++ unions
      ++ aliases
      ++ values


unionToDoc :: Text -> Docs.Union -> P.Doc
unionToDoc name (Docs.Union tvars ctors) =
  let
    setup =
      P.text "type" <+> text name <+> P.hsep (map text tvars)

    separators =
      map P.text ("=" : repeat "|")

    ctorDoc (ctor, tipes) =
      typeDoc (Type.App (Type.Type ctor) tipes)
  in
    P.hang setup 4 (P.sep (zipWith (<+>) separators (map ctorDoc ctors)))


aliasToDoc :: Text -> Docs.Alias -> P.Doc
aliasToDoc name (Docs.Alias tvars tipe) =
  let
    typeAlias =
      P.text "type" <+> P.text "alias"

    declaration =
      typeAlias <+> text name <+> P.hsep (map text tvars) <+> P.equals
  in
    P.hang declaration 4 (typeDoc tipe)


valueToDoc :: Text -> Docs.Value Type.Type -> P.Doc
valueToDoc name value =
  case value of
    Docs.Value tipe ->
      text name <+> P.colon <+> typeDoc tipe

    Docs.Infix tipe _ _ ->
      P.text "(" <> text name <> P.text ")" <+> P.colon <+> typeDoc tipe


typeDoc :: Type.Type -> P.Doc
typeDoc tipe =
  P.text (Type.toString Type.OneLine tipe)


text :: Text -> P.Doc
text txt =
  P.text (Text.unpack txt)
