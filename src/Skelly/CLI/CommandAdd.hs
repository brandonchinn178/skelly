module Skelly.CLI.CommandAdd (commandAdd) where

import Skelly.CLI.Command

commandAdd :: Command
commandAdd =
  Command
    { cmdName = "add"
    , cmdDesc = "Add a dependency to hsproject.toml"
    , cmdParse = pure ()
    , cmdRun = run
    }

run :: () -> SharedOptions -> IO ()
run () _ = putStrLn "TODO: add"
