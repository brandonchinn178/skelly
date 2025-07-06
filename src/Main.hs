{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import Skelly.CLI.Command
import Skelly.CLI.CommandAdd
import Skelly.CLI.CommandBuild
import Skelly.CLI.CommandClean
import Skelly.CLI.CommandLock
import Skelly.CLI.CommandRun
import Skelly.CLI.CommandTest
import Skelly.CLI.Service qualified as CLI
import Skelly.Core.Logging qualified as Logging

data BaseOptions = BaseOptions
  { logOptions :: Logging.Options
  -- TODO: allow user to configure the index provider + hackage opts
  , hackageOptions :: PackageIndex.Options -- PackageIndex.defaultHackageOptions
  }

commandMain :: Command ()
commandMain =
  CommandGroup
    { cmdName = "skelly"
    , cmdDesc = ""
    , cmdChildren =
        [ commandAdd
        , commandBuild
        , commandClean
        , commandLock
        , commandRun
        , commandTest
        ]
    , cmdExtraOptions = do
        optionVerbose <-
          Option
            { longName = "verbose"
            , shortName = Just 'v'
            , helpText = "Output more verbose logs"
            , optParser = boolFlag
            }
        -- Logging.Options
        --   { logLevel = if cliVerbose then Logging.LevelDebug else Logging.LevelInfo
        --   }
        pure BaseOptions{..}
    }

main :: IO ()
main = do
  ParsedCommand{cmdAction, cmdParsedOptions} <- parseCommand commandMain
  service <- runRegistry cmdParsedOptions loadService
  cmdAction service
