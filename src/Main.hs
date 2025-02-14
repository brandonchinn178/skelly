{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import Skelly.CLI.Command
import Skelly.CLI.CommandAdd
import Skelly.CLI.CommandBuild
import Skelly.CLI.CommandClean
import Skelly.CLI.CommandRun
import Skelly.CLI.CommandTest
import Skelly.CLI.Service qualified as CLI
import Skelly.Core.Logging qualified as Logging

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
      hsubparser . mconcat $
        [ command cmdName . info (fromCommand cmd) . mconcat $
            [ progDesc cmdDesc
            ]
        | cmd@Command{..} <-
            [ commandAdd
            , commandBuild
            , commandLock
            , commandClean
            , commandRun
            , commandTest
            ]
        ]

    -- TODO: allow -v after subcommand
    verboseParser =
      switch . mconcat $
        [ long "verbose"
        , short 'v'
        , help "Output more verbose logs"
        ]

main :: IO ()
main = do
  Options{..} <- parseOptions
  service <-
    CLI.initService
      CLI.Options
        { logOptions =
            Logging.Options
              { logLevel = if cliVerbose then Logging.LevelDebug else Logging.LevelInfo
              }
        }
  executeCommand service cliCommand
