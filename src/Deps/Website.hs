{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Website
  ( getPkgInfo
  , getNewPackages
  , getAllPackages
  )
  where

import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi

import Elm.Package (Name, Version)
import qualified Elm.Package as Package

import qualified Elm.Project as Project
import qualified Reporting.Task as Task



-- GET PACKAGE INFO


getPkgInfo :: Name -> Version -> Task.Task Project.PkgInfo
getPkgInfo name version =
  let
    path =
      "packages/" ++ Package.toUrl name
      ++ "/" ++ Package.versionToString version
      ++ "/elm.json"

    fetchContent =
      Task.fetch path [] $ \request manager ->
        do  response <- Client.httpLbs request manager
            return (Client.responseBody response)
  in
    do  json <- fetchContent
        case Project.parse json of
          Right (Project.Pkg info) ->
            return info

          _ ->
            Task.throw (error "TODO getPkgInfo")



-- EASY REQUESTS


versions :: Name -> Task.Task (Maybe [Version])
versions name =
  fetchBinary "versions" [("name", Package.toString name)]


permissions :: Name -> Task.Task Bool
permissions name =
  fetchBinary "permissions" [("name", Package.toString name)]


fetchBinary :: (Binary.Binary a) => String -> [(String,String)] -> Task.Task a
fetchBinary path params =
  Task.fetch path params $ \request manager ->
    do  response <- Client.httpLbs request manager
        return $ Binary.decode $ Client.responseBody response



-- NEW PACKAGES


getNewPackages :: Integer -> Task.Task (Integer, [(Name, Version)])
getNewPackages time =
  fetchBinary "new-packages" [("since", show time)]


getAllPackages :: Task.Task (Integer, Map.Map Name [Version])
getAllPackages =
  fetchBinary "new-packages" []



-- REGISTER PACKAGES


register :: Name -> Version -> Task.Task ()
register name version =
  let
    params =
      [ ("name", Package.toString name)
      , ("version", Package.versionToString version)
      ]

    files =
      [ Multi.partFileSource "documentation" "documentation.json"
      , Multi.partFileSource "description" "description.json"
      , Multi.partFileSource "readme" "README.md"
      ]
  in
    Task.fetch "register" params $ \request manager ->
      do  request' <- Multi.formDataBody files request
          let request'' = request' { Client.responseTimeout = Nothing }
          Client.httpLbs request'' manager
          return ()

