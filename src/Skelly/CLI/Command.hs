{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.Command (
  Command (..),
  SharedOptions (..),

  -- * ParsedCommand
  ParsedCommand,
  fromCommand,
  executeCommand,
) where

import Options.Applicative qualified as Opt
import Skelly.Core.Logging (LogLevel)

data Command =
  forall opts.
  Command
  { cmdName :: String
  , cmdDesc :: String
  , cmdParse :: Opt.Parser opts
  , cmdRun :: opts -> SharedOptions -> IO ()
  }

data SharedOptions = SharedOptions
  { logLevel :: LogLevel
  }

newtype ParsedCommand = ParsedCommand (SharedOptions -> IO ())

fromCommand :: Command -> Opt.Parser ParsedCommand
fromCommand Command{..} = ParsedCommand . cmdRun <$> cmdParse

executeCommand :: ParsedCommand -> SharedOptions -> IO ()
executeCommand (ParsedCommand f) = f
