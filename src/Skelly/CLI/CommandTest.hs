module Skelly.CLI.CommandTest (commandTest) where

import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI

commandTest :: Command
commandTest =
  Command
    { cmdName = "test"
    , cmdDesc = "Run tests"
    , cmdParse = pure ()
    , cmdExec = execute
    }

execute :: CLI.Service -> () -> IO ()
execute _ () = putStrLn "TODO: test"
