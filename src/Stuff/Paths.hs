module Stuff.Paths
  ( dir
  , pkgInfo
  , deps
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

