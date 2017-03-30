{-# OPTIONS_GHC -Wall #-}
module Generate.Organize
  ( organize
  )
  where


import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Package as Pkg

import qualified Elm.Project.Summary as Summary
import qualified File.Crawl as Crawl
import qualified File.IO as IO
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- ORGANIZE


organize :: Summary.Summary -> Crawl.Graph () -> Task.Task Obj.Graph
organize (Summary.Summary _ _ _ _ deps) (Crawl.Graph locals _ _ _) =
  do  localObjs <- Obj.unions <$> traverse loadModuleObj (Map.keys locals)
      foreignObjs <- Obj.unions <$> traverse loadPackageObj (Map.toList deps)
      return (Obj.union localObjs foreignObjs)


loadModuleObj :: Module.Raw -> Task.Task Obj.Graph
loadModuleObj name =
  IO.readBinary (Paths.elmo name)


loadPackageObj :: ( Pkg.Name, (Pkg.Version, deps) ) -> Task.Task Obj.Graph
loadPackageObj ( name, (version,_) ) =
  do  dir <- Task.getPackageCacheDirFor name version
      IO.readBinary (dir </> "objs.dat")
