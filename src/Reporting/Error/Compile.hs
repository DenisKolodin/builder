{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Compile
  ( Error(..)
  )
  where


import qualified Elm.Compiler as Compiler



-- ERRORS


data Error =
  Error
    { _path :: FilePath
    , _errors :: [Compiler.Error]
    }
