{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.Command (
  Command (..),

  -- * ParsedCommand
  ParsedCommand,
  fromCommand,
  executeCommand,
) where

import Options.Applicative qualified as Opt
import Skelly.CLI.Service qualified as CLI

data Command =
  forall opts.
  Command
  { cmdName :: String
  , cmdDesc :: String
  , cmdParse :: Opt.Parser opts
  , cmdExec :: CLI.Service -> opts -> IO ()
  }

newtype ParsedCommand = ParsedCommand (CLI.Service -> IO ())

fromCommand :: Command -> Opt.Parser ParsedCommand
fromCommand Command{..} = ParsedCommand . flip cmdExec <$> cmdParse

executeCommand :: CLI.Service -> ParsedCommand -> IO ()
executeCommand service (ParsedCommand f) = f service
