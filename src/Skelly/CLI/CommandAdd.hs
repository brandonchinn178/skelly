module Skelly.CLI.CommandAdd (commandAdd) where

import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI

commandAdd :: Command
commandAdd =
  Command
    { cmdName = "add"
    , cmdDesc = "Add a dependency to hsproject.toml"
    , cmdParse = pure ()
    , cmdExec = execute
    }

execute :: CLI.Service -> () -> IO ()
execute _ () = putStrLn "TODO: add"
