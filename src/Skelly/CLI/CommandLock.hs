{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.CommandLock (commandLock) where

import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI
import Skelly.Core.Lock qualified as Lock

commandLock :: Command
commandLock =
  Command
    { cmdName = "lock"
    , cmdDesc = "Manage lock files"
    , cmdParse =
        pure Lock.Options
    , cmdExec = execute
    }

execute :: CLI.Service -> Lock.Options -> IO ()
execute CLI.Service{..} = Lock.run service
  where
    service = Lock.initService loggingService
