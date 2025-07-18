{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Skelly.CLI.CommandClean (commandClean) where

import Skelly.CLI.Command
import Skelly.CLI.CommandBase
import Skelly.Core.Service (IsService (..))

commandClean :: CommandSpec '[BaseOptions]
commandClean =
  CommandSpec
    { cmdName = "clean"
    , cmdDesc = "Clean all build artifacts"
    , cmdImpl = execute
    , cmdOptions = pure ()
    }

data Service = Service

instance IsService opts Service where
  initService = pure Service

execute :: Service -> () -> IO ()
execute _ () = putStrLn "TODO: clean"
