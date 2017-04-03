{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Output
  ( generate
  )
  where


import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import qualified System.IO as IO

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Crawl as Crawl
import qualified File.Hash as Hash
import qualified Generate.BoilerPlate as BoilerPlate
import qualified Generate.Organize as Organize
import qualified Reporting.Task as Task



-- GENERATE


generate :: Summary.Summary -> Crawl.Graph () -> Module.Raw -> Task.Task ()
generate summary@(Summary.Summary _ project _ _ deps) graph name =
  do  cacheDir <- Task.getPackageCacheDir

      objectGraph <- Organize.organize summary graph
      let root = Obj.root (Project.getName project) name
      let (natives, builder) = Compiler.generate (Obj.symbolTable Map.empty) objectGraph root

      hash <- liftIO $ IO.withBinaryFile "temp.js" IO.WriteMode $ \handle ->
        do  let append = appendKernel handle cacheDir deps
            state <- foldM append Hash.starter natives
            Hash.putBuilder handle state builder

      liftIO $ Dir.renameFile "temp.js" (Hash.toString hash ++ ".js")



-- APPEND KERNELS


appendKernel :: IO.Handle -> FilePath -> Summary.DepsGraph -> Hash.State -> Module.Canonical -> IO Hash.State
appendKernel handle cacheDir deps state name =
  Hash.append handle (pathTo cacheDir deps name) state


pathTo :: FilePath -> Summary.DepsGraph -> Module.Canonical -> FilePath
pathTo cacheDir deps (Module.Canonical pkg name) =
  cacheDir
  </> Pkg.toFilePath pkg
  </> Pkg.versionToString (fst (deps ! pkg))
  </> "src"
  </> Module.nameToPath name
  <.> "js"



{-- GENERATE HTML

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.List as List

import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Text as Blaze
import Text.RawString.QQ (r)


html :: Text.Text -> Module.Raw -> LText.Text
html generatedJavaScript moduleName =
  Blaze.renderMarkup $
    H.docTypeHtml $ do
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title (H.toHtml (Module.nameToString moduleName))
        H.style $ Blaze.preEscapedToMarkup
            ("html,head,body { padding:0; margin:0; }\n\
             \body { font-family: calibri, helvetica, arial, sans-serif; }" :: Text.Text)
        H.script ! A.type_ "text/javascript" $
            Blaze.preEscapedToMarkup generatedJavaScript
      H.body $ do
        H.script ! A.type_ "text/javascript" $
            Blaze.preEscapedToMarkup ("Elm." ++ Module.nameToString moduleName ++ ".fullscreen()")



-- FOOTER


createFooter
  :: Bool
  -> Map.Map TMP.CanonicalModule Module.Interface
  -> [TMP.CanonicalModule]
  -> Text.Text
createFooter debugMode canonicalInterfaces rootModules =
  let
    interfaces =
      Map.mapKeys TMP.simplifyModuleName canonicalInterfaces

    exportChunks =
      map
        (exportProgram debugMode interfaces)
        (List.sort (map TMP.simplifyModuleName rootModules))
  in
    Text.pack $
      "var Elm = {};\n"
      ++ unlines exportChunks
      ++ footerClose


exportProgram
  :: Bool
  -> Map.Map Module.Canonical Module.Interface
  -> Module.Canonical
  -> String
exportProgram debugMode interfaces canonicalName@(Module.Canonical _ rawName) =
  let
    program =
      Text.unpack (Module.qualifiedVar canonicalName "main")

    moduleName =
      map Text.unpack (Text.splitOn "." rawName)

    object =
      objectFor moduleName

    name =
      Module.nameToString rawName

    debugArg =
      if debugMode then createDebugMetadata interfaces canonicalName else "undefined"
  in
    setup moduleName
    ++ "if (typeof " ++ program ++ " !== 'undefined') {\n    "
    ++ program ++ "(" ++ object ++ ", '" ++ name ++ "', " ++ debugArg ++ ");\n}"


createDebugMetadata :: Map.Map Module.Canonical Module.Interface -> Module.Canonical -> String
createDebugMetadata interfaces canonicalName =
  let
    metadataFields =
      [ "versions" .= Json.object [ "elm" .= Elm.version ]
      , "types" .= Module.programTypes interfaces canonicalName
      ]
  in
    Json.object metadataFields
      |> Json.encode
      |> LText.decodeUtf8
      |> LText.replace "\\u003e" ">"
      |> LText.unpack


setup :: [String] -> String
setup moduleName =
  let
    create path =
      let
        jsPath =
          objectFor path
      in
        jsPath ++ " = " ++ jsPath ++ " || {};"

    paths =
      tail (List.inits moduleName)
  in
    unlines (map create paths)


objectFor :: [String] -> String
objectFor names =
  let
    brackets :: String -> String
    brackets name =
      "['" ++ name ++ "']"
  in
    "Elm" ++ concatMap brackets names


-}