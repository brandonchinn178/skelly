module Skelly.CLI.CommandTest (commandTest) where

import Skelly.CLI.Command

commandTest :: Command
commandTest =
  Command
    { cmdName = "test"
    , cmdDesc = "Run tests"
    , cmdParse = pure ()
    , cmdRun = run
    }

run :: () -> SharedOptions -> IO ()
run () _ = putStrLn "TODO: test"
