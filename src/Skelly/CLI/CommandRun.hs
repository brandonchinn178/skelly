{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Skelly.CLI.CommandRun (commandRun) where

import Skelly.CLI.Command
import Skelly.CLI.CommandBase
import Skelly.Core.Service (IsService (..))

commandRun :: CommandSpec '[BaseOptions]
commandRun =
  CommandSpec
    { cmdName = "run"
    , cmdDesc = "Run an executable"
    , cmdImpl = execute
    , cmdOptions = pure ()
    }

data Service = Service

instance IsService opts Service where
  initService = pure Service

execute :: Service -> () -> IO ()
execute _ () = putStrLn "TODO: run"
