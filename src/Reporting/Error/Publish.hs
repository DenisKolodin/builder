{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Publish
  ( Error(..)
  , toReport
  )
  where

import qualified Text.PrettyPrint.ANSI.Leijen as P

import Deps.Diff (Magnitude)
import qualified Elm.Package as Pkg
import qualified Reporting.Error.Help as Help



-- ERRORS


data Error
  = Application
  | NotInitialVersion
  | AlreadyPublished Pkg.Version
  | InvalidBump Pkg.Version Pkg.Version
  | BadBump Pkg.Version Pkg.Version Magnitude Pkg.Version Magnitude
  | NoSummary
  | NoExposed
  | NoReadme
  | ShortReadme
  | NoLicense
  | MissingTag Pkg.Version
  | NoGit
  | LocalChanges Pkg.Version



-- TO REPORT


toReport :: Error -> Help.Report
toReport err =
  case err of
    Application ->
      Help.report "UNPUBLISHABLE" Nothing "I cannot publish applications, only packages!" []

    NotInitialVersion ->
      error "TODO NotInitialVersion"

    AlreadyPublished vsn ->
      Help.docReport "ALREADY PUBLISHED" Nothing
        ( P.vcat
            [ P.fillSep
                [ "Version", P.green (P.text (Pkg.versionToString vsn))
                , "has", "already", "been", "published.", "You", "cannot"
                , "publish", "it", "again!"
                ]
            , "Try using the `bump` command:"
            ]
        )
        [ P.dullyellow $ P.indent 4 $ P.text "elm bump"
        , Help.reflow $
            "It computes the version number based on API changes, ensuring\
            \ that no breaking changes end up in PATCH releases!"
        ]

    InvalidBump statedVersion latestVersion ->
      error "TODO" Nothing
        ( "Your elm.json says the next version should be "
          ++ Pkg.versionToString statedVersion ++ ", but that is not valid\
          \ based on the previously published versions."
        )
        [ Help.reflow $
            "Generally, you want to put the most recently published version ("
            ++ Pkg.versionToString latestVersion
            ++ " for this package) in your elm.json and run `elm bump` to figure out what should come next."
        ]

    BadBump old new magnitude realNew realMagnitude ->
      error "TODO" Nothing
        ( "Your elm.json says the next version should be "
          ++ Pkg.versionToString new ++ ", indicating a " ++ error "TODO magnitude" magnitude
          ++ " change to the public API. This does not match the API diff given by:"
        )
        [ P.indent 4 $ P.text $
            "elm diff " ++ Pkg.versionToString old

        , Help.reflow $
          "This command says this is a " ++ error "TODO realMagnitude" realMagnitude
          ++ " change, so the next version should be "
          ++ Pkg.versionToString realNew
          ++ ". Double check everything to make sure you are publishing what you want!"
        , Help.reflow $
            "Also, next time use `elm bump` and I'll figure all this out for you!"
        ]

    NoSummary ->
      Help.docReport "NO SUMMARY" (Just "elm.json")
        ( P.fillSep $
            [ "To", "publish", "a", "package,", "your", "elm.json", "must"
            , "have", "a", P.dullyellow "\"summary\"", "field", "that", "gives"
            , "a", "consice", "overview", "of", "your", "project."
            ]
        )
        [ Help.reflow $
            "The summary must be less than 80 characters. It should describe\
            \ the concrete use of your package as clearly and as plainly as possible."
        ]

    NoExposed ->
      Help.docReport "NO EXPOSED MODULES" (Just "elm.json")
        ( P.fillSep $
            [ "To", "publish", "a", "package,", "the"
            , P.dullyellow "\"exposed-modules\"", "field", "of", "your"
            , "elm.json", "must", "list", "at", "least", "one", "module."
            ]
        )
        [ Help.reflow $
            "What is the point of a package that has no modules?!"
        ]

    NoReadme ->
      badReadmeReport "NO README" $
        "Every published package must have a helpful README.md\
        \ file, but I do not see one in your project."

    ShortReadme ->
      badReadmeReport "SHORT README" $
        "This README.md is too short. Having more details will help\
        \ people assess your package quickly and fairly."

    NoLicense ->
      Help.report "NO LICENSE FILE" (Just "LICENSE")
        "By publishing a package you are inviting the Elm community to build\
        \ upon your work. But without knowing your license, we have no idea if\
        \ that is legal!"
        [ Help.reflow $
            "Once you pick an OSI approved license from <https://spdx.org/licenses/>,\
            \ you must share that choice in two places. First, the license\
            \ identifier must appear in your elm.json file. Second, the full\
            \ license text must appear in the root of your project in a file\
            \ named LICENSE. Add that file and you will be all set!"
        ]

    MissingTag version ->
      let vsn = Pkg.versionToString version in
      Help.docReport "NO TAG" Nothing
        ( P.fillSep $
            [ "Packages", "must", "be", "tagged", "in", "git,", "but", "I"
            , "cannot", "find", "a", P.green (P.text vsn), "tag."
            ]
        )
        [ P.vcat
            [ "These tags make it possible to find this specific version on GitHub."
            , "To tag the most recent commit and push it to GitHub, run this:"
            ]
        , P.indent 4 $ P.dullyellow $ P.vcat $ map P.text $
            [ "git tag -a " ++ vsn ++ " -m \"release version " ++ vsn ++ "\""
            , "git push origin " ++ vsn
            ]
        , "The -m flag is for a helpful message. Try to make it more informative!"
        ]

    NoGit ->
      Help.report "NO GIT" Nothing
        "I searched your PATH environment variable for `git` and could not\
        \ find it. Is it available through your PATH?"
        [ Help.reflow $
            "Who cares about this? Well, I currently use `git` to check if there\
            \ are any local changes in your code. Local changes are a good sign\
            \ that some important improvements have gotten mistagged, so this\
            \ check can be extremely helpful for package authors!"
        , Help.note $
            "We plan to do this without the `git` binary in a future release."
        ]

    LocalChanges version ->
      let vsn = Pkg.versionToString version in
      Help.docReport "LOCAL CHANGES" Nothing
        ( P.fillSep $
            [ "The", "code", "tagged", "as", P.green (P.text vsn), "in"
            , "git", "does", "not", "match", "the", "code", "in", "your"
            , "working", "directory.", "This", "means", "you", "have"
            , "commits", "or", "local", "changes", "that", "are", "not"
            , "going", "to", "be", "published!"
            ]
        )
        [ Help.note $
            "If you are sure everything is in order, you can run `git checkout "
            ++ vsn ++ "` and publish your code from there."
        ]


badReadmeReport :: String -> String -> Help.Report
badReadmeReport title summary =
  Help.report title (Just "README.md") summary
    [ Help.reflow $
        "When people look at your README, they are wondering:"
    , P.vcat
        [ "  - What does this package even do?"
        , "  - Will it help me solve MY problems?"
        ]
    , Help.reflow $
        "So I recommend starting your README with a small example of the\
        \ most common usage scenario. Show people what they can expect if\
        \ they learn more!"
    , Help.note $
        "By publishing your package, you are inviting thousands of developers to\
        \ invest time in understanding and building upon your work. I find it\
        \ helpful to ask myself, “Am I being respectful of their time?” How can I\
        \ improve my package to save them time? In that spirit, thoughtful\
        \ documentation is an obvious requirement for any worthwhile package."
    ]


-- By publishing your package, you are inviting thousands of developers to
-- invest time in understanding and building on top of your work. The whole
-- point of sharing your work is to SAVE them time. Part of that is writing
-- great code, but that is not enough. A great package MUST have great docs.
--
-- Folks are wondering everything from “Is this package relevant to my work?”
-- all the way to “How do the most sophisticated functions in the package
-- interact with each other?” Great documentation anticipates these questions,
-- helping people achieve their goals more quickly.

-- Since you are inviting folks to invest in your work, you should be respectful
-- of their time. That is the point of documentation!

-- (Separately, writing great docs usually improves your API a bit. When some
-- detail is hard to explain, that is an excellent sign that the API itself
-- can be done better in some way. Many core Elm libraries have been improved this way!)
