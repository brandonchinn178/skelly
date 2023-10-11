module Skelly.CLI.CommandClean (commandClean) where

import Skelly.CLI.Command

commandClean :: Command
commandClean =
  Command
    { cmdName = "clean"
    , cmdDesc = "Clean all build artifacts"
    , cmdParse = pure ()
    , cmdRun = run
    }

run :: () -> SharedOptions -> IO ()
run () _ = putStrLn "TODO: clean"
