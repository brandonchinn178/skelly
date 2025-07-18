{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import Skelly.CLI.Command
import Skelly.CLI.CommandAdd
import Skelly.CLI.CommandBase
import Skelly.CLI.CommandBuild
import Skelly.CLI.CommandClean
import Skelly.CLI.CommandLock
import Skelly.CLI.CommandRun
import Skelly.CLI.CommandTest
import Skelly.CLI.Service qualified as CLI
import Skelly.Core.Logging qualified as Logging

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
    , cmdExtraOptions = parseBaseOptions
    }

main :: IO ()
main = do
  ParsedCommand{cmdAction, cmdParsedOptions} <- parseCommand commandMain
  service <- runRegistry cmdParsedOptions loadService
  cmdAction service
