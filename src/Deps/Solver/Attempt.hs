module Deps.Solver.Attempt
  ( Answer(..)
  , addToApp
  , addToPkg
  , view
  )
  where


import Control.Monad (filterM, forM, msum)
import Control.Monad.Trans (lift)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Text as Text
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<>), (<+>))

import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import Deps.Explorer (Explorer)
import qualified Deps.Explorer as Explorer
import Deps.Solver.Internal (Solver)
import qualified Deps.Solver.Internal as Solver
import Elm.Project.Json (AppInfo(..), PkgInfo(..))
import Elm.Project.Constraint (Constraint)
import qualified Elm.Project.Constraint as Con
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- ANSWER


data Answer a =
  Answer
    { _notLatest :: Maybe Version
    , _changes :: Map Name (Change a)
    }


makeAnswer :: (Eq a) => Name -> Version -> Map Name a -> Map Name a -> Explorer (Answer a)
makeAnswer name version old new =
  do  versions <- Explorer.getVersions name
      let latest = maximum versions
      let notLatest = if latest == version then Nothing else Just latest
      return $ Answer notLatest (detectChanges old new)



-- CHANGES


data Change a
  = Insert a
  | Change a a
  | Remove a


detectChanges :: (Eq a) => Map Name a -> Map Name a -> Map Name (Change a)
detectChanges old new =
  Map.merge
    (Map.mapMissing (\_ v -> Remove v))
    (Map.mapMissing (\_ v -> Insert v))
    (Map.zipWithMaybeMatched keepChange)
    old
    new


keepChange :: (Eq v) => k -> v -> v -> Maybe (Change v)
keepChange _ old new =
  if old == new then
    Nothing
  else
    Just (Change old new)



-- ADD TO APP


addToApp :: Name -> AppInfo -> Task.Task (Answer Version)
addToApp pkg info@(AppInfo _ _ deps tests trans) =
  Explorer.run $
    do  let old = Map.unions [ deps, tests, trans ]
        result <- Solver.run (addToAppHelp pkg info)
        case result of
          Just new ->
            makeAnswer pkg (new ! pkg) old new

          Nothing ->
            do  badNames <- filterM isBadElm (pkg : Map.keys old)
                lift $ Task.throw (Error.NoSolution badNames)


addToAppHelp :: Name -> AppInfo -> Solver (Map Name Version)
addToAppHelp pkg (AppInfo _ _ deps tests trans) =
  let
    directs =
      Map.union deps tests

    everything =
      Map.union directs trans

    try constraints =
      Solver.solve $ Map.insert pkg Con.anything constraints
  in
    msum $ map try $
      [ Map.map Con.exactly everything
      , Map.map Con.exactly directs
      , Map.map Con.untilNextMinor directs
      , Map.map Con.untilNextMajor directs
      , Map.map (\_ -> Con.anything) directs
      ]



-- ADD TO PKG


addToPkg :: Name -> PkgInfo -> Task.Task (Answer Constraint)
addToPkg pkg info@(PkgInfo _ _ _ _ _ deps tests _ _) =
  Explorer.run $
    do  let old = Map.union deps tests
        result <- Solver.run (addToPkgHelp pkg info)
        case result of
          Just (vsn, new) ->
            makeAnswer pkg vsn old new

          Nothing ->
            do  let pkgs = Map.keys deps ++ Map.keys tests
                badNames <- filterM isBadElm (pkg : pkgs)
                lift $ Task.throw (Error.NoSolution badNames)


addToPkgHelp :: Name -> PkgInfo -> Solver (Version, Map Name Constraint)
addToPkgHelp pkg (PkgInfo _ _ _ _ _ deps tests _ _) =
  do  let directs = Map.union deps tests
      let newCons = Map.insert pkg Con.anything directs
      solution <- Solver.solve newCons
      let vsn = solution ! pkg
      let con = Con.untilNextMajor vsn
      return ( vsn, Map.insert pkg con directs )



-- FAILURE HINTS


isBadElm :: Name -> Explorer Bool
isBadElm name =
  do  versions <- Explorer.getVersions name

      elmVersions <- forM versions $ \vsn ->
        Explorer._elm <$> Explorer.getConstraints name vsn

      return (not (any Con.goodElm elmVersions))



-- VIEW


view :: (a -> String) -> Map Name (Change a) -> P.Doc
view toString changes =
  let
    widths =
      Map.foldrWithKey (widen toString) (Widths 0 0 0) changes

    changeDocs =
      Map.foldrWithKey (addChange toString widths) (Docs [] [] []) changes
  in
    viewChangeDocs changeDocs



-- VIEW CHANGE DOCS


data ChangeDocs =
  Docs
    { _doc_inserts :: [P.Doc]
    , _doc_changes :: [P.Doc]
    , _doc_removes :: [P.Doc]
    }


viewChangeDocs :: ChangeDocs -> P.Doc
viewChangeDocs (Docs inserts changes removes) =
  P.indent 2 $ P.vcat $ concat $
    [ viewNonZero "Add:"    inserts
    , viewNonZero "Change:" changes
    , viewNonZero "Remove:" removes
    ]


viewNonZero :: String -> [P.Doc] -> [P.Doc]
viewNonZero title entries =
  if null entries then
    []
  else
    P.text "" : P.text title : reverse entries



-- VIEW CHANGE


addChange :: (a -> String) -> Widths -> Name -> Change a -> ChangeDocs -> ChangeDocs
addChange toString widths name change (Docs inserts changes removes) =
  case change of
    Insert new ->
      Docs (viewInsert toString widths name new : inserts) changes removes

    Change old new ->
      Docs inserts (viewChange toString widths name old new : changes) removes

    Remove old ->
      Docs inserts changes (viewRemove toString widths name old : removes)


viewInsert :: (a -> String) -> Widths -> Name -> a -> P.Doc
viewInsert toString (Widths nameWidth leftWidth _) name new =
  P.green (P.text "+") <+> viewName nameWidth name <+> pad leftWidth (toString new)


viewChange :: (a -> String) -> Widths -> Name -> a -> a -> P.Doc
viewChange toString (Widths nameWidth leftWidth rightWidth) name old new =
  P.hsep
    [ P.yellow (P.text "+")
    , viewName nameWidth name
    , pad leftWidth (toString old)
    , P.text "=>"
    , pad rightWidth (toString new)
    ]


viewRemove :: (a -> String) -> Widths -> Name -> a -> P.Doc
viewRemove toString (Widths nameWidth leftWidth _) name old =
  P.red (P.text "+") <+> viewName nameWidth name <+> pad leftWidth (toString old)


viewName :: Int -> Name -> P.Doc
viewName width name =
  P.fill (width + 3) (P.text (Pkg.toString name))


pad :: Int -> String -> P.Doc
pad width string =
  P.text (replicate (width - length string) ' ') <> P.text string



-- WIDTHS


data Widths =
  Widths
    { _name :: !Int
    , _left :: !Int
    , _right :: !Int
    }


widen :: (a -> String) -> Name -> Change a -> Widths -> Widths
widen toString pkg change (Widths name left right) =
  let
    toLength a =
      length (toString a)

    newName =
      max name (Text.length (Pkg.toText pkg))
  in
    case change of
      Insert new ->
        Widths newName (max left (toLength new)) right

      Change old new ->
        Widths newName (max left (toLength old)) (max right (toLength new))

      Remove old ->
        Widths newName (max left (toLength old)) right
