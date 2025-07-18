{-# LANGUAGE DataKinds #-}

module Skelly.CLI.CommandLock (commandLock) where

import Skelly.CLI.Command
import Skelly.CLI.CommandBase (BaseOptions)
import Skelly.Core.Lock qualified as Lock

commandLock :: CommandSpec '[BaseOptions]
commandLock =
  CommandSpec
    { cmdName = "lock"
    , cmdDesc = "Manage lock files"
    , cmdImpl = Lock.run
    , cmdOptions =
        pure Lock.Options
    }
