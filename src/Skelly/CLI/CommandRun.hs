module Skelly.CLI.CommandRun (commandRun) where

import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI

commandRun :: Command
commandRun =
  Command
    { cmdName = "run"
    , cmdDesc = "Run an executable"
    , cmdParse = pure ()
    , cmdExec = execute
    }

execute :: CLI.Service -> () -> IO ()
execute _ () = putStrLn "TODO: run"
