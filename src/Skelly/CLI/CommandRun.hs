module Skelly.CLI.CommandRun (commandRun) where

import Skelly.CLI.Command

commandRun :: Command
commandRun =
  Command
    { cmdName = "run"
    , cmdDesc = "Run an executable"
    , cmdParse = pure ()
    , cmdRun = run
    }

run :: () -> SharedOptions -> IO ()
run () _ = putStrLn "TODO: run"
