{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Diff
  ( diff
  , PackageChanges(..)
  , ModuleChanges(..)
  , Changes(..)
  , Magnitude(..)
  , moduleChangeMagnitude
  , magnitudeToString
  , toMagnitude
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
  deriving (Eq, Ord)


magnitudeToString :: Magnitude -> String
magnitudeToString magnitude =
  case magnitude of
    PATCH ->
      "PATCH"

    MINOR ->
      "MINOR"

    MAJOR ->
      "MAJOR"


bump :: PackageChanges -> Pkg.Version -> Pkg.Version
bump changes version =
  case toMagnitude changes of
    PATCH ->
      Pkg.bumpPatch version

    MINOR ->
      Pkg.bumpMinor version

    MAJOR ->
      Pkg.bumpMajor version


toMagnitude :: PackageChanges -> Magnitude
toMagnitude (PackageChanges added changed removed) =
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
