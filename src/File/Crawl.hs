{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Crawl
  ( Graph(..)
  , Info(..)
  , crawl
  , crawlFromSource
  )
  where

import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Monad (foldM)
import Control.Monad.Except (liftIO)
import qualified Data.Graph as Graph
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import Elm.Project.Json (Project)
import Elm.Project.Summary (Summary(..))
import qualified File.Args as Args
import qualified File.Find as Find
import qualified File.IO as IO
import qualified Generate.Plan as Plan
import qualified Reporting.Error as Error
import qualified Reporting.Error.Crawl as E
import qualified Reporting.Task as Task



-- CRAWL


crawl :: Summary -> Args.Args FilePath -> Task.Task (Graph ())
crawl summary args =
  case args of
    Args.Pkg names ->
      do  let roots = map (Unvisited Nothing) names
          let graph = freshGraph (Args.Pkg names) Map.empty
          dfs summary roots graph

    Args.App plan@(Plan.Plan cache pages _ _ _) ->
      do  let names = cache : map Plan._elm pages
          let roots = map (Unvisited Nothing) names
          let graph = freshGraph (Args.App plan) Map.empty
          dfs summary roots graph

    Args.Roots (path :| []) ->
      crawlHelp summary =<< readFileHeader1 summary path

    Args.Roots paths ->
      do  headers <- traverse (readFileHeaderN summary) paths
          let names = NonEmpty.map fst headers
          let roots = map (Unvisited Nothing) (NonEmpty.toList names)
          let graph = freshGraph (Args.Roots names) (Map.fromList (NonEmpty.toList headers))
          dfs summary roots graph


crawlFromSource :: Summary -> FilePath -> Text -> Task.Task (Graph ())
crawlFromSource summary@(Summary _ project _ _ _) fakePath source =
  crawlHelp summary =<<
    atRoot (parseHeader project fakePath source)


crawlHelp :: Summary -> ( Maybe Module.Raw, Info ) -> Task.Task (Graph ())
crawlHelp summary ( maybeName, info@(Info _ _ deps) ) =
  do  let name = maybe "Main" id maybeName
      let args = Args.Roots (name :| [])
      let roots = map (Unvisited maybeName) deps
      let graph = freshGraph args (Map.singleton name info)
      dfs summary roots graph


atRoot :: CTask a -> Task.Task a
atRoot task =
  Task.mapError Error.BadCrawlRoot task



-- DEPTH FIRST SEARCH


dfs :: Summary -> [Unvisited] -> WorkGraph -> Task.Task (Graph ())
dfs summary unvisited startGraph =
  do  chan <- liftIO newChan

      graph <- dfsHelp summary chan 0 Set.empty unvisited startGraph

      if Map.null (_problems graph)
        then
          do  checkForCycles graph
              return (graph { _problems = () })
        else
          Task.throw (Error.BadCrawl (_problems graph))


type FileResult = Either (Module.Raw, E.Error) Asset


dfsHelp :: Summary -> Chan FileResult -> Int -> Set.Set Module.Raw -> [Unvisited] -> WorkGraph -> Task.Task WorkGraph
dfsHelp summary chan oldPending oldSeen unvisited graph =
  do  (seen, pending) <-
        foldM (crawlNew summary chan) (oldSeen, oldPending) unvisited

      if pending == 0
        then return graph
        else
          do  asset <- liftIO $ readChan chan
              case asset of
                Right (Local name info) ->
                  do  let newGraph = graph { _locals = Map.insert name info (_locals graph) }
                      let deps = map (Unvisited (Just name)) (_imports info)
                      dfsHelp summary chan (pending - 1) seen deps newGraph

                Right (Kernel name path) ->
                  do  let kernels = Map.insert name path (_kernels graph)
                      let newGraph = graph { _kernels = kernels }
                      dfsHelp summary chan (pending - 1) seen [] newGraph

                Right (Foreign name pkg) ->
                  do  let foreigns = Map.insert name pkg (_foreigns graph)
                      let newGraph = graph { _foreigns = foreigns }
                      dfsHelp summary chan (pending - 1) seen [] newGraph

                Left (name, err) ->
                  do  let problems = Map.insert name err (_problems graph)
                      let newGraph = graph { _problems = problems }
                      dfsHelp summary chan (pending - 1) seen [] newGraph


crawlNew
  :: Summary
  -> Chan FileResult
  -> (Set.Set Module.Raw, Int)
  -> Unvisited
  -> Task.Task (Set.Set Module.Raw, Int)
crawlNew summary chan (seen, n) unvisited@(Unvisited _ name) =
  if Set.member name seen then
    return (seen, n)
  else
    do  Task.workerChan chan (crawlFile summary unvisited)
        return (Set.insert name seen, n + 1)



-- DFS STATE


data Graph problems =
  Graph
    { _args :: Args.Args Module.Raw
    , _locals :: Map.Map Module.Raw Info
    , _kernels :: Map.Map Module.Raw FilePath
    , _foreigns :: Map.Map Module.Raw Pkg.Package
    , _problems :: problems
    }


data Info =
  Info
    { _path :: FilePath
    , _source :: Text
    , _imports :: [Module.Raw]
    }


type WorkGraph = Graph (Map.Map Module.Raw E.Error)


freshGraph :: Args.Args Module.Raw -> Map.Map Module.Raw Info -> WorkGraph
freshGraph args locals =
  Graph args locals Map.empty Map.empty Map.empty



-- DFS WORKER


data Unvisited =
  Unvisited
    { _parent :: Maybe Module.Raw
    , _name :: Module.Raw
    }


data Asset
  = Local Module.Raw Info
  | Kernel Module.Raw FilePath
  | Foreign Module.Raw Pkg.Package


crawlFile :: Summary -> Unvisited -> Task.Task_ (Module.Raw, E.Error) Asset
crawlFile summary (Unvisited maybeParent name) =
  Task.mapError ((,) name) $
    do  asset <- Find.find summary maybeParent name
        case asset of
          Find.Local path ->
            readModuleHeader summary name path

          Find.Kernel path ->
            return (Kernel name path)

          Find.Foreign pkg ->
            return (Foreign name pkg)



-- READ HEADER


type CTask a = Task.Task_ E.Error a


readModuleHeader :: Summary -> Module.Raw -> FilePath -> CTask Asset
readModuleHeader summary expectedName path =
  do  source <- liftIO (IO.readUtf8 path)
      (maybeName, info) <- parseHeader (_project summary) path source
      name <- checkName path expectedName maybeName
      return (Local name info)


readFileHeader1 :: Summary -> FilePath -> Task.Task (Maybe Module.Raw, Info)
readFileHeader1 summary path =
  do  source <- liftIO (IO.readUtf8 path)
      atRoot $ parseHeader (_project summary) path source


readFileHeaderN :: Summary -> FilePath -> Task.Task (Module.Raw, Info)
readFileHeaderN summary path =
  do  source <- liftIO (IO.readUtf8 path)
      (maybeName, info) <- atRoot $ parseHeader (_project summary) path source
      case maybeName of
        Nothing ->
          error ("TODO module at " ++ path ++ " needs a name")

        Just name ->
          return (name, info)


-- TODO get regions on data extracted here
parseHeader :: Project -> FilePath -> Text -> CTask (Maybe Module.Raw, Info)
parseHeader project path source =
  case Compiler.parseDependencies (Project.getName project) source of
    Right (tag, maybeName, deps) ->
      do  checkTag project path tag
          return ( maybeName, Info path source deps )

    Left msg ->
      Task.throw (E.BadHeader path msg)


checkName :: FilePath -> Module.Raw -> Maybe Module.Raw -> CTask Module.Raw
checkName path expectedName maybeName =
  case maybeName of
    Nothing ->
      Task.throw (E.NoName path expectedName)

    Just actualName ->
      if expectedName == actualName
        then return expectedName
        else Task.throw (E.BadName path actualName)


checkTag :: Project -> FilePath -> Compiler.Tag -> CTask ()
checkTag project path tag =
  case tag of
    Compiler.Normal ->
      return ()

    Compiler.Port ->
      case project of
        Project.App _ _ ->
          return ()

        Project.Pkg _ ->
          Task.throw (E.PortsInPackage path)

    Compiler.Effect ->
      if Project.getEffect project then
        return ()

      else
        Task.throw (E.EffectsUnexpected path)



-- DETECT CYCLES


checkForCycles :: Graph a -> Task.Task ()
checkForCycles (Graph _ locals _ _ _) =
  let
    toNode (name, info) =
      (name, name, _imports info)

    components =
      Graph.stronglyConnComp (map toNode (Map.toList locals))
  in
    mapM_ checkComponent components


checkComponent :: Graph.SCC Module.Raw -> Task.Task ()
checkComponent scc =
  case scc of
    Graph.AcyclicSCC _ ->
      return ()

    Graph.CyclicSCC names ->
      Task.throw (Error.Cycle names)
