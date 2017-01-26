module Reporting.Error.Project
  ( Error(..)
  )
  where


import Data.Text (Text)



data Error
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

