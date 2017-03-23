{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module File.Generate
  ( generate
  , Config(..)
  )
  where


import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS (defaultChunkSize)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified System.IO as IO

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg

import qualified Elm.Project.BuildPlan as BP
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Crawl as Crawl
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- GENERATE


type Results = Map.Map Module.Raw Compiler.Result


data Config
  = Only [Module.Raw]
  | Everything Summary.Summary


generate :: Crawl.Graph () -> Results -> Config -> Task.Task ()
generate graph results config =
  case config of
    Only roots ->
      do  let (elmos, pkgs) = sortElmos graph results roots
          error "TODO"

    Everything (Summary.Summary _ project _ _) ->
      case project of
        Project.Pkg _ ->
          return ()

        Project.App _ Nothing ->
          return ()

        Project.App _ (Just (BP.BuildPlan cache pages bundles endpoint outputDir)) ->
          do  let (elmos, _) = sortElmos graph results (map BP._elm pages)
              liftIO (write elmos)



-- TOPOLOGICAL SORT


data Elmo = Source BS.Builder | File FilePath


sortElmos :: Crawl.Graph () -> Results -> [Module.Raw] -> ([Elmo], Set.Set (Pkg.Name, Pkg.Version))
sortElmos graph results roots =
  let
    (State pkgs elmos _) =
      List.foldl' (sort graph results) (State Set.empty [] Set.empty) roots
  in
    (elmos, pkgs)


data State =
  State
    { _pkgs :: !(Set.Set (Pkg.Name, Pkg.Version))
    , _elmos :: ![Elmo]
    , _visited :: !(Set.Set Module.Raw)
    }


sort :: Crawl.Graph () -> Results -> State -> Module.Raw -> State
sort graph@(Crawl.Graph locals natives foreigns _) results state@(State pkgs elmos visited) name =
  if Set.member name visited then
    state

  else
    case Map.lookup name locals of
      Just info ->
        sortLocal graph results name info state

      Nothing ->
        case Map.lookup name foreigns of
          Just pkg ->
            State (Set.insert pkg pkgs) elmos (Set.insert name visited)

          Nothing ->
            case Map.lookup name natives of
              Just path ->
                State pkgs (File path : elmos) (Set.insert name visited)

              Nothing ->
                error "compiler bug is manifesting in File.Generate"


sortLocal :: Crawl.Graph () -> Results -> Module.Raw -> Crawl.Info -> State -> State
sortLocal graph results name (Crawl.Info path _ imports) state =
  let
    (State pkgs elmos visited) =
      List.foldl' (sort graph results) state imports

    elmo =
      case Map.lookup name results of
        Nothing ->
          File (Paths.elmo name)

        Just (Compiler.Result _ _ js) ->
          Source js
  in
    State pkgs (elmo : elmos) (Set.insert name visited)



-- WRITE JS FILE


write :: [Elmo] -> IO ()
write elmos =
  do  hash <-
        IO.withBinaryFile "temp.js" IO.WriteMode $ \handle ->
          toHash <$> foldM (writeElmo handle) (ShaState 0 SHA.sha1Incremental) elmos

      Dir.renameFile "temp.js" (hash ++ ".js")


data ShaState =
  ShaState
    { _length :: !Int
    , _decoder :: !(Binary.Decoder SHA.SHA1State)
    }


toHash :: ShaState -> String
toHash (ShaState len decoder) =
  SHA.showDigest $ SHA.completeSha1Incremental decoder len


writeElmo :: IO.Handle -> ShaState -> Elmo -> IO ShaState
writeElmo handle state elmo =
  case elmo of
    Source builder ->
      foldM (put handle) state $
        LBS.toChunks (BS.toLazyByteString builder)

    File path ->
      IO.withBinaryFile path IO.ReadMode $ \handle2 ->
        copyFile handle handle2 state


put :: IO.Handle -> ShaState -> BS.ByteString -> IO ShaState
put handle (ShaState len decoder) chunk =
  do  BS.hPut handle chunk
      return $ ShaState (len + BS.length chunk) (Binary.pushChunk decoder chunk)


copyFile :: IO.Handle -> IO.Handle -> ShaState -> IO ShaState
copyFile output input state =
  do  chunk <- BS.hGet input BS.defaultChunkSize
      if BS.null chunk
        then return state
        else copyFile output input =<< put output state chunk



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


footerClose :: String
footerClose = [r|
if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);
|]



-- HEADER


header :: Text.Text
header = [r|
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}
|]


-}