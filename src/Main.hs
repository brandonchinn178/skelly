{-# LANGUAGE RecordWildCards #-}

import Options.Applicative
import Skelly.CLI.Command
import Skelly.CLI.CommandAdd
import Skelly.CLI.CommandBuild
import Skelly.CLI.CommandClean
import Skelly.CLI.CommandRun
import Skelly.CLI.CommandTest
import Skelly.CLI.Service qualified as CLI
import Skelly.Core.Logging (LogLevel (..))

data Options = Options
  { cliCommand :: ParsedCommand
  , cliVerbose :: Bool
  }

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
        <*> verboseParser

    commandParser =
      subparser . mconcat $
        [ command cmdName . info (fromCommand cmd) . mconcat $
            [ progDesc cmdDesc
            ]
        | cmd@Command{..} <-
            [ commandAdd
            , commandBuild
            , commandClean
            , commandRun
            , commandTest
            ]
        ]

    verboseParser =
      switch . mconcat $
        [ long "verbose"
        , short 'v'
        , help "Output more verbose logs"
        ]

main :: IO ()
main = do
  Options{..} <- parseOptions
  let service =
        CLI.initService
          CLI.Options
            { logLevel = if cliVerbose then LevelDebug else LevelInfo
            }
  executeCommand service cliCommand
