module Elm.Install
  ( install
  )
  where


import Control.Monad (filterM, forM, msum, void)
import Control.Monad.Except (catchError, lift, liftIO)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Text as Text
import qualified System.IO as IO
import Text.PrettyPrint.ANSI.Leijen ((<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import Deps.Explorer (Explorer)
import qualified Deps.Explorer as Explorer
import qualified Deps.Verify as Verify
import qualified Deps.Solver as Solver
import Elm.Project.Constraint (Constraint)
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Error as Error
import qualified Reporting.Task as Task



-- INSTALL


install :: Name -> Summary.Summary -> Task.Task ()
install pkg (Summary.Summary root oldProject _ _ _) =
  let
    setup project =
      do  liftIO $ Project.write root project
          void $ Verify.verify root project

    revert err =
      do  liftIO $ Project.write root oldProject
          Task.throw err
  in
    do  newProject <- makePlan pkg oldProject
        Task.withApproval (setup newProject `catchError` revert)



-- MAKE PLAN


makePlan :: Name -> Project.Project -> Task.Task Project.Project
makePlan pkg project =
  case project of
    Project.App info@(Project.AppInfo elm srcDirs deps test _) plan ->
      do  changes <- addToApp pkg info
          liftIO $ printPlan Pkg.versionToString changes
          let solution = Map.mapMaybe keepNew changes
          let newDeps = Map.intersection solution (Map.insert pkg Pkg.dummyVersion deps)
          let newTest = Map.intersection solution test
          let newTrans = Map.difference solution (Map.union newDeps newTest)
          let newInfo = Project.AppInfo elm srcDirs newDeps newTest newTrans
          return $ Project.App newInfo plan

    Project.Pkg info@(Project.PkgInfo _ _ _ _ _ deps test _ _) ->
      do  changes <- addToPkg pkg info
          liftIO $ printPlan Con.toString changes
          let solution = Map.mapMaybe keepNew changes
          return $ Project.Pkg $
            info
              { Project._pkg_deps =
                  Map.intersection solution (Map.insert pkg Con.anything deps)
              , Project._pkg_test_deps =
                  Map.intersection solution test
              }




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


keepNew :: Change a -> Maybe a
keepNew change =
  case change of
    Insert a ->
      Just a

    Change _ a ->
      Just a

    Remove _ ->
      Nothing



-- ADD TO APP


addToApp :: Name -> Project.AppInfo -> Task.Task (Map Name (Change Version))
addToApp pkg info@(Project.AppInfo _ _ deps tests trans) =
  Explorer.run $
    do  let old = Map.unions [ deps, tests, trans ]
        result <- Solver.run (addToAppHelp pkg info)
        case result of
          Just new ->
            return $ detectChanges old new

          Nothing ->
            do  badNames <- filterM isBadElm (pkg : Map.keys old)
                lift $ Task.throw (Error.NoSolution badNames)


addToAppHelp :: Name -> Project.AppInfo -> Solver.Solver (Map Name Version)
addToAppHelp pkg (Project.AppInfo _ _ deps tests trans) =
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


addToPkg :: Name -> Project.PkgInfo -> Task.Task (Map Name (Change Constraint))
addToPkg pkg info@(Project.PkgInfo _ _ _ _ _ deps tests _ _) =
  Explorer.run $
    do  let old = Map.union deps tests
        result <- Solver.run (addToPkgHelp pkg info)
        case result of
          Just new ->
            return $ detectChanges old new

          Nothing ->
            do  let pkgs = Map.keys deps ++ Map.keys tests
                badNames <- filterM isBadElm (pkg : pkgs)
                lift $ Task.throw (Error.NoSolution badNames)


addToPkgHelp :: Name -> Project.PkgInfo -> Solver.Solver (Map Name Constraint)
addToPkgHelp pkg (Project.PkgInfo _ _ _ _ _ deps tests _ _) =
  do  let directs = Map.union deps tests
      let newCons = Map.insert pkg Con.anything directs
      solution <- Solver.solve newCons
      let con = Con.untilNextMajor (solution ! pkg)
      return $ Map.insert pkg con directs



-- FAILURE HINTS


isBadElm :: Name -> Explorer Bool
isBadElm name =
  do  versions <- Explorer.getVersions name

      elmVersions <- forM versions $ \vsn ->
        Explorer._elm <$> Explorer.getConstraints name vsn

      return (not (any Con.goodElm elmVersions))



-- VIEW


printPlan :: (a -> String) -> Map Name (Change a) -> IO ()
printPlan toString changes =
  let
    widths =
      Map.foldrWithKey (widen toString) (Widths 0 0 0) changes

    changeDocs =
      Map.foldrWithKey (addChange toString widths) (Docs [] [] []) changes
  in
    P.displayIO IO.stdout $ P.renderPretty 1 80 $
      P.vcat
        [ P.cyan (P.text "I can give you way more control over dependencies.") <+> P.text "Learn about it here:"
        , P.text "<https://github.com/evancz/cli/TODO>"
        , P.text "That approach is way nicer, especially if you are doing something complex!"
        , P.text ""
        , P.text "That said, here is my naive plan:"
        , viewChangeDocs changeDocs
        , P.text ""
        , P.text "Do you approve? [Y/n]: "
        ]



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
