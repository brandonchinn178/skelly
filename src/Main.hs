{-# LANGUAGE RecordWildCards #-}

import Options.Applicative
import Skelly

data Options = Options
  { cliCommand :: Command
  }

data Command
  = Add
  | Build
  | Clean
  | Run
  | Test

parseOptions :: IO Options
parseOptions =
  execParser $
    info (optionsParser <**> helper) . mconcat $
      [ fullDesc
      , header "skelly - An opinioned Haskell build system"
      ]
  where
    optionsParser =
      Options
        <$> commandParser

    commandParser =
      subparser . mconcat $
        [ command "add" . info (pure Add) . mconcat $
            [ progDesc "Add a dependency to hsproject.toml"
            ]
        , command "build" . info (pure Build) . mconcat $
            [ progDesc "Build a Haskell project"
            ]
        , command "clean" . info (pure Clean) . mconcat $
            [ progDesc "Clean build artifacts"
            ]
        , command "run" . info (pure Run) . mconcat $
            [ progDesc "Run a built executable"
            ]
        , command "test" . info (pure Test) . mconcat $
            [ progDesc "Run the project tests"
            ]
        ]

main :: IO ()
main = do
  Options{..} <- parseOptions
  case cliCommand of
    Add -> runAdd
    Build -> runBuild
    Clean -> runClean
    Run -> runRun
    Test -> runTest
