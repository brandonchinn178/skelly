{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.CommandLock (commandLock) where

import Skelly.CLI.Command
import Skelly.CLI.CommandBase (BaseOptions)
import Skelly.Core.Lock qualified as Lock
import Skelly.Core.Solver qualified as Solver

commandLock :: Command '[BaseOptions]
commandLock =
  Command
    { cmdName = "lock"
    , cmdDesc = "Manage lock files"
    , cmdParse =
        pure Lock.Options
    , cmdExec = Lock.run
    }
