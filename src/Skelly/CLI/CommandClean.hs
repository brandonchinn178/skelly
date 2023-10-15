module Skelly.CLI.CommandClean (commandClean) where

import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI

commandClean :: Command
commandClean =
  Command
    { cmdName = "clean"
    , cmdDesc = "Clean all build artifacts"
    , cmdParse = pure ()
    , cmdExec = execute
    }

execute :: CLI.Service -> () -> IO ()
execute _ () = putStrLn "TODO: clean"
