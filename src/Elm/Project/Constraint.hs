{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Constraint
  ( Constraint
  , fromText
  , toString
  , toText
  , satisfies
  , check
  , intersect
  , goodElm
  , defaultElm
  , untilNextMajor
  , untilNextMinor
  , expand
  )
  where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Control.Monad (guard)
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



-- TEXT CONVERSION


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
      do  lo <- versionFromText lower
          lop <- opFromText lowerOp
          hop <- opFromText upperOp
          hi <- versionFromText upper
          guard (lo <= hi)
          return (Range lo lop hop hi)

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



-- IS SATISFIED


satisfies :: Constraint -> Package.Version -> Bool
satisfies constraint version =
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



-- INTERSECT


intersect :: Constraint -> Constraint -> Maybe Constraint
intersect (Range lo lop hop hi) (Range lo_ lop_ hop_ hi_) =
  let
    (newLo, newLop) =
      case compare lo lo_ of
        LT -> (lo_, lop_)
        EQ -> (lo, if elem Less [lop,lop_] then Less else LessOrEqual)
        GT -> (lo, lop)

    (newHi, newHop) =
      case compare hi hi_ of
        LT -> (hi, hop)
        EQ -> (hi, if elem Less [hop, hop_] then Less else LessOrEqual)
        GT -> (hi_, hop_)
  in
    if newLo <= newHi then
      Just (Range newLo newLop newHop newHi)
    else
      Nothing



-- ELM CONSTRAINT


goodElm :: Constraint -> Bool
goodElm constraint =
  satisfies constraint Compiler.version


defaultElm :: Constraint
defaultElm =
  if Package._major Compiler.version > 0
    then untilNextMajor Compiler.version
    else untilNextMinor Compiler.version



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



-- JSON CONVERSION


instance Json.ToJSON Constraint where
  toJSON constraint =
    Json.String (toText constraint)


instance Json.FromJSON Constraint where
  parseJSON =
    Json.withText "Constraint" $ \text ->
      case fromText text of
        Just constraint ->
          return constraint

        Nothing ->
          fail "bad constraint, need something like \"1.2.1 <= v < 2.0.0\""
