{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Find
  ( Error(..)
  )
  where


import qualified Elm.Package as Pkg



-- ERRORS


data Error
  = NotFound -- TODO suggest other names
  | Duplicates [FilePath] [Pkg.Name]

