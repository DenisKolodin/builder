{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Assets
  ( Error(..)
  , ProjectError(..)
  )
  where


import Data.Text (Text)

import qualified Elm.Package as Pkg



-- ERRORS


data Error
  = CorruptProject FilePath ProjectError
  | CorruptDocumentation String
  | CorruptVersionCache Pkg.Name
  | PackageNotFound Pkg.Name [Pkg.Name]
  | CorruptBinary FilePath



-- PROJECT ERRORS


data ProjectError
  = BadJson
  | BadType
  | Missing Text
  | BadFlag Text
  | BadLicense [Text]
  | BadSummary
  | BadAppDeps Text
  | BadAppDepName Text Text
  | BadPkgDeps Text
  | BadPkgDepName Text Text
  | BadExposed Text
  | BadExposedName Text Text
  | BadDir Text
  | BadVersion (Maybe Text) Text
  | BadConstraint (Maybe Text) Text
  | BadRepo Text



{-
    CorruptJson _url ->
      error "TODO"

    CorruptProject path problem ->
      Message
        ( "Your " ++ path ++ " is invalid."
        )
        [ error "TODO" problem
        ]

    CorruptDocumentation problem ->
      Message
        ( "I was able to produce documentation for your package, but it is not valid.\
          \ My guess is that the elm-package and elm-make on your PATH are not from the\
          \ same version of Elm, but it could be some other similarly crazy thing."
        )
        [ text problem
        ]

    CorruptVersionCache name ->
      Message
        ( "Your .elm/packages/ directory may be corrupted. I was led to believe\
          \ that " ++ Pkg.toString name ++ " existed, but I could not find anything\
          \ when I went to look up the published versions of this package."
        )
        []


    PackageNotFound package suggestions ->
      Message
        ( "Could not find any packages named " ++ Pkg.toString package ++ "."
        )
        [ text $ "Here are some packages that have similar names:"
        , indent 4 $ vcat $ map (text . Pkg.toString) suggestions
        , text $ "Maybe you want one of those?"
        ]
-}