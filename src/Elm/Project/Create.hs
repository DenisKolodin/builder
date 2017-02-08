import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler


createProject :: Task.Task Project
createProject =
  do  liftIO $ putStr newProjectMessage
      yes <- liftIO CL.yesOrNo
      if yes then createProjectYes else liftIO createProjectNo


newProjectMessage :: String
newProjectMessage =
  "It looks like you are starting a brand new Elm project!\n\
  \\n\
  \A typical Elm application is set up like this:\n\
  \\n\
  \  elm.json        # dependencies and settings for your project\n\
  \  src/Main.elm    # a src/ directory where all the Elm code lives\n\
  \\n\
  \Do you want me to set that up for you? [Y/n] "


createProjectNo :: IO a
createProjectNo =
  do  putStrLn "\nOkay, maybe later!"
      Exit.exitSuccess


createProjectYes :: Task.Task Project
createProjectYes =
  do  project <- App <$> defaultAppInfo
      liftIO $ do
        write Assets.projectPath project
        Dir.createDirectoryIfMissing True "src"
        BS.writeFile ("src" </> "Main.elm") defaultProgram
      return project



-- DEFAULTS


defaultAppInfo :: Task.Task AppInfo
defaultAppInfo =
  do  deps <- error "TODO getDefaultDeps"
      return $
        AppInfo
          { _app_elm_version = Compiler.version
          , _app_dependencies = deps
          , _app_test_deps = Map.empty
          , _app_exact_deps = Map.empty
          , _app_source_dir = "src"
          , _app_cache_dir = "elm-cache"
          , _app_output_dir = "elm-output"
          , _app_bundles = error "TODO _app_bundles"
          }


defaultProgram :: BS.ByteString
defaultProgram =
  "-- Learn about how this works in the official Elm guide:\n\
  \-- https://guide.elm-lang.org\n\
  \\n\
  \import Html exposing (Html, beginnerProgram, div, button, text)\n\
  \import Html.Events exposing (onClick)\n\
  \\n\
  \\n\
  \\n\
  \main =\n\
  \  beginnerProgram { model = model, view = view, update = update }\n\
  \\n\
  \\n\
  \\n\
  \-- MODEL\n\
  \\n\
  \\n\
  \type alias Model =\n\
  \  Int\n\
  \\n\
  \\n\
  \model : Model\n\
  \model =\n\
  \  0\n\
  \\n\
  \\n\
  \\n\
  \-- UPDATE\n\
  \\n\
  \\n\
  \type Msg\n\
  \  = Increment\n\
  \  | Decrement\n\
  \\n\
  \\n\
  \update : Msg -> Model -> Model\n\
  \update msg model =\n\
  \  case msg of\n\
  \    Increment ->\n\
  \      model + 1\n\
  \\n\
  \    Decrement ->\n\
  \      model - 1\n\
  \\n\
  \\n\
  \\n\
  \-- VIEW\n\
  \\n\
  \\n\
  \view : Model -> Html Msg\n\
  \view model =\n\
  \  div []\n\
  \    [ button [ onClick Decrement ] [ text \"-\" ]\n\
  \    , div [] [ text (toString model) ]\n\
  \    , button [ onClick Increment ] [ text \"+\" ]\n\
  \    ]\n"
