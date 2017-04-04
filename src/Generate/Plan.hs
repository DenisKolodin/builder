{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Plan
  ( Plan(..)
  , Page(..)
  , parse
  )
  where

import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Reporting.Error as Error
import qualified Reporting.Error.Assets as E
import qualified Reporting.Task as Task
import qualified Json.Decode as D



-- PLAN


data Plan =
  Plan
    { _cache :: Module.Raw
    , _pages :: [Page]
    , _bundles :: [[Pkg.Name]]
    , _endpoint :: Text
    , _output_dir :: FilePath
    }


data Page =
  Page
    { _elm :: Module.Raw
    , _css :: [Text]
    , _js :: [Text]
    }



-- PARSE


parse :: BS.ByteString -> Task.Task Plan
parse bytestring =
  case D.parse planDecoder bytestring of
    Left Nothing ->
      throw E.BadSyntax

    Left (Just err) ->
      throw (E.BadStructure err)

    Right plan ->
      case detectProblems plan of
        Nothing ->
          return plan

        Just _ ->
          throw E.BadContent


throw :: E.JsonProblem -> Task.Task a
throw problem =
  Task.throw (Error.Assets (E.BadBuildPlan problem))



-- JSON


planDecoder :: D.Decoder Plan
planDecoder =
  Plan
    <$> D.field "cache" Module.decoder
    <*> D.field "pages" (D.list pageDecoder)
    <*> D.field "bundles" (D.list (D.list Pkg.decoder))
    <*> D.field "endpoint" D.text
    <*> D.field "output-directory" (D.map Text.unpack D.text)


pageDecoder :: D.Decoder Page
pageDecoder =
  Page
    <$> D.field "elm" Module.decoder
    <*> D.field "css" (D.list D.text)
    <*> D.field "js" (D.list D.text)



-- VALIDATE


detectProblems :: Plan -> Maybe a
detectProblems (Plan cache pages bundles _ _) =
  let
    uniquePages =
      Set.fromList (map _elm pages)

    allPackages =
      concat bundles

    uniquePackages =
      Set.fromList allPackages
  in
    if Set.member cache uniquePages then
      Just (error "TODO cache must not be a page")

    else if Set.size uniquePages < length pages then
      Just (error "TODO repeat pages")

    else if Set.size uniquePackages < length allPackages then
      Just (error "TODO repeat bundles")

    else
      Nothing
