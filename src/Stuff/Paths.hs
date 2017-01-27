module Stuff.Paths
  ( dir
  , pkgInfo
  , deps
  , ifaces
  )
  where


import System.FilePath ((</>))



-- PATHS


dir :: FilePath
dir =
  "elm-stuff"


pkgInfo :: FilePath
pkgInfo =
  dir </> "elm.dat"


deps :: FilePath
deps =
  dir </> "deps.dat"


ifaces :: FilePath
ifaces =
  dir </> "ifaces.dat"

