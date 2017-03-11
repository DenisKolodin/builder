{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Compile
  ( Error(..)
  , toDocs
  )
  where


import Data.Text (Text)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import qualified Elm.Compiler as Compiler



-- ERRORS


data Error =
  Error
    { _path :: FilePath
    , _source :: Text
    , _localizer :: Compiler.Localizer
    , _errors :: [Compiler.Error]
    }


toDocs :: Error -> [Doc]
toDocs (Error path source localizer errors) =
  map (Compiler.errorToDoc localizer path source) errors
