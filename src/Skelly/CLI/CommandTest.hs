{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Skelly.CLI.CommandTest (commandTest) where

import Skelly.CLI.Command
import Skelly.CLI.CommandBase
import Skelly.Core.Service (IsService (..))

commandTest :: CommandSpec '[BaseOptions]
commandTest =
  CommandSpec
    { cmdName = "test"
    , cmdDesc = "Run tests"
    , cmdImpl = execute
    , cmdOptions = pure ()
    }

data Service = Service

instance IsService opts Service where
  initService = pure Service

execute :: Service -> () -> IO ()
execute _ () = putStrLn "TODO: test"
