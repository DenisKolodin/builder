{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Crawl
  ( Graph(..)
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

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import Elm.Project.Summary (Summary(..))
import qualified File.Args as Args
import qualified File.Find as Find
import qualified File.Header as Header
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
      crawlHelp summary =<< Header.readOneFile summary path

    Args.Roots paths ->
      do  headers <- Header.readManyFiles summary paths
          let deps = concatMap (Header._imports . snd) (NonEmpty.toList headers)
          let names = NonEmpty.map fst headers
          let unvisited = map (Unvisited Nothing) deps
          let graph = freshGraph (Args.Roots names) (Map.fromList (NonEmpty.toList headers))
          dfs summary unvisited graph


crawlFromSource :: Summary -> Text -> Task.Task (Graph ())
crawlFromSource summary@(Summary _ project _ _ _) source =
  crawlHelp summary =<<
    Header.readSource project source


crawlHelp :: Summary -> ( Maybe Module.Raw, Header.Info ) -> Task.Task (Graph ())
crawlHelp summary ( maybeName, info@(Header.Info _ _ _ deps) ) =
  do  let name = maybe "Main" id maybeName
      let args = Args.Roots (name :| [])
      let roots = map (Unvisited maybeName) deps
      let graph = freshGraph args (Map.singleton name info)
      dfs summary roots graph



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
                Right (Local name info@(Header.Info _ _ _ imports)) ->
                  do  let newGraph = graph { _locals = Map.insert name info (_locals graph) }
                      let deps = map (Unvisited (Just name)) imports
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
    , _locals :: Map.Map Module.Raw Header.Info
    , _kernels :: Map.Map Module.Raw FilePath
    , _foreigns :: Map.Map Module.Raw Pkg.Package
    , _problems :: problems
    }


type WorkGraph = Graph (Map.Map Module.Raw E.Error)


freshGraph :: Args.Args Module.Raw -> Map.Map Module.Raw Header.Info -> WorkGraph
freshGraph args locals =
  Graph args locals Map.empty Map.empty Map.empty



-- DFS WORKER


data Unvisited =
  Unvisited
    { _parent :: Maybe Module.Raw
    , _name :: Module.Raw
    }


data Asset
  = Local Module.Raw Header.Info
  | Kernel Module.Raw FilePath
  | Foreign Module.Raw Pkg.Package


crawlFile :: Summary -> Unvisited -> Task.Task_ (Module.Raw, E.Error) Asset
crawlFile summary (Unvisited maybeParent name) =
  Task.mapError ((,) name) $
    do  asset <- Find.find summary maybeParent name
        case asset of
          Find.Local path ->
            uncurry Local <$> Header.readModule summary name path

          Find.Kernel path ->
            return (Kernel name path)

          Find.Foreign pkg ->
            return (Foreign name pkg)



-- DETECT CYCLES


checkForCycles :: Graph a -> Task.Task ()
checkForCycles (Graph _ locals _ _ _) =
  let
    toNode (name, Header.Info _ _ _ imports) =
      (name, name, imports)

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
