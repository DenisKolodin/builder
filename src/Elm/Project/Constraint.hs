{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Constraint
  ( Constraint
  , fromText
  , toString
  , toText
  , untilNextMajor
  , untilNextMinor
  , expand
  , defaultElmVersion
  , isSatisfied
  , check
  , errorMessage
  )
  where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Data.Aeson as Json
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Elm.Package as Package
import qualified Elm.Compiler as Compiler



-- CONSTRAINTS


data Constraint
    = Range Package.Version Op Op Package.Version
    deriving (Eq, Generic)


data Op
  = Less
  | LessOrEqual
  deriving (Eq, Generic)


instance Binary Constraint
instance Binary Op



-- CREATE CONSTRAINTS


untilNextMajor :: Package.Version -> Constraint
untilNextMajor version =
  Range version LessOrEqual Less (Package.bumpMajor version)


untilNextMinor :: Package.Version -> Constraint
untilNextMinor version =
  Range version LessOrEqual Less (Package.bumpMinor version)


expand :: Constraint -> Package.Version -> Constraint
expand constraint@(Range lower lowerOp upperOp upper) version
  | version < lower =
      Range version LessOrEqual upperOp upper

  | version > upper =
      Range lower lowerOp Less (Package.bumpMajor version)

  | otherwise =
      constraint



-- ELM CONSTRAINT


defaultElmVersion :: Constraint
defaultElmVersion =
  if Package._major Compiler.version > 0
    then untilNextMajor Compiler.version
    else untilNextMinor Compiler.version



-- CHECK IF SATISFIED


isSatisfied :: Constraint -> Package.Version -> Bool
isSatisfied constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
        isLess lowerOp lower version
          &&
        isLess upperOp version upper


isLess :: (Ord a) => Op -> (a -> a -> Bool)
isLess op =
  case op of
    Less ->
      (<)

    LessOrEqual ->
      (<=)


check :: Constraint -> Package.Version -> Ordering
check constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
      if not (isLess lowerOp lower version) then
        LT

      else if not (isLess upperOp version upper) then
        GT

      else
        EQ



-- STRING CONVERSION


toString :: Constraint -> String
toString constraint =
  Text.unpack (toText constraint)


toText :: Constraint -> Text
toText constraint =
  case constraint of
    Range lower lowerOp upperOp upper ->
      Text.intercalate " "
        [ Package.versionToText lower
        , opToText lowerOp
        , "v"
        , opToText upperOp
        , Package.versionToText upper
        ]


opToText :: Op -> Text
opToText op =
  case op of
    Less ->
      "<"

    LessOrEqual ->
      "<="


fromText :: Text -> Maybe Constraint
fromText text =
  case Text.splitOn " " text of
    [lower, lowerOp, "v", upperOp, upper] ->
      Range
        <$> versionFromText lower
        <*> opFromText lowerOp
        <*> opFromText upperOp
        <*> versionFromText upper

    _ ->
      Nothing


versionFromText :: Text -> Maybe Package.Version
versionFromText text =
  either (const Nothing) Just $
    Package.versionFromText text


opFromText :: Text -> Maybe Op
opFromText text =
  case text of
    "<=" ->
      Just LessOrEqual

    "<" ->
      Just Less

    _ ->
      Nothing



-- JSON CONVERSION


instance Json.ToJSON Constraint where
  toJSON constraint =
    Json.String (toText constraint)


instance Json.FromJSON Constraint where
  parseJSON (Json.String text) =
    case fromText text of
      Just constraint ->
        return constraint

      Nothing ->
        fail $ errorMessage Nothing (Text.unpack text)

  parseJSON _ =
    fail "constraint must be a string that looks something like \"1.2.1 <= v < 2.0.0\"."


errorMessage :: Maybe String -> String -> String
errorMessage maybeContext rawConstraint =
  unlines
    [ "Ran into invalid constraint \"" ++ rawConstraint ++ "\"" ++ maybe "" (" for " ++) maybeContext
    , ""
    , "It should look something like \"1.2.1 <= v < 2.0.0\", with no extra or missing"
    , "spaces. The middle letter needs to be a 'v' as well."
    , ""
    , "Upper and lower bounds are required so that bounds represent the maximum range"
    , "known to work. You do not want to promise users your library will work with"
    , "4.0.0 that version has not been tested!"
    ]