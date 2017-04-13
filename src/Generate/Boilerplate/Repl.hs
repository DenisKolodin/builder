

nodeHeader :: Text.Text
nodeHeader =
  Text.concat
    [ "process.on('uncaughtException', function(err) {\n\
      \  process.stderr.write(err.toString());\n\
      \  process.exit(1);\n\
      \});\n\
      \var ", lastVar, ";\n"
    ]


nodeFooter :: Text.Text
nodeFooter =
  Text.concat
    [ "\n"
    , "if (typeof ", lastVar, " !== 'undefined') {\n"
    , "  console.log(_elm_lang$core$Native_Utils.toString(", lastVar, "));\n"
    , "}\n"
    ]



printIfNeeded :: String -> IO ()
printIfNeeded rawValue =
  case rawValue of
    "" ->
      return ()

    _ ->
      do  tipe <- getType

          let value =
                init rawValue

          let isTooLong =
                List.isInfixOf "\n" value
                  || List.isInfixOf "\n" tipe
                  || length value + 3 + length tipe > 80

          let tipeAnnotation =
                if isTooLong then
                  "\n    : " ++ List.intercalate "\n      " (lines tipe)

                else
                  " : " ++ tipe

          putStrLn (value ++ tipeAnnotation)