{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Compile
  ( Error(..)
  )
  where


import Data.Text (Text)

import qualified Elm.Compiler as Compiler



-- ERRORS


data Error =
  Error
    { _path :: FilePath
    , _source :: Text
    , _errors :: [Compiler.Error]
    }
