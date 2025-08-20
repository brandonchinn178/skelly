{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Skelly.CLI.Command
import Skelly.CLI.CommandAdd
import Skelly.CLI.CommandBase
import Skelly.CLI.CommandBuild
import Skelly.CLI.CommandClean
import Skelly.CLI.CommandLock
import Skelly.CLI.CommandRun
import Skelly.CLI.CommandTest
import Skelly.Core.Service (loadServiceIO)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import UnliftIO.Exception (SomeException (..), catch, displayException)

commandMain :: CommandSpec '[]
commandMain =
  CommandGroupSpec
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
  service <- loadServiceIO cmdParsedOptions
  cmdAction service `catch` \(SomeException e) -> do
    -- unwrap SomeException to avoid outputting backtrace
    hPutStrLn stderr (displayException e)
    exitFailure
