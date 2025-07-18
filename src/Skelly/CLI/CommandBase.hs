{-# LANGUAGE ApplicativeDo #-}

module Skelly.CLI.CommandBase (
  BaseOptions,
  parseBaseOptions,
) where

import Options.Applicative qualified as Opt
import Skelly.CLI.Command

type BaseOptions = HList '[Logging.Options, PackageIndex.Options]


parseBaseOptions :: Opt.Parser BaseOptions
parseBaseOptions = do
  optionVerbose <-
    Option
      { longName = "verbose"
      , shortName = Just 'v'
      , helpText = "Output more verbose logs"
      , optParser = boolFlag
      }
  let logOptions =
        Logging.Options
          { logLevel = if optionVerbose then Logging.LevelDebug else Logging.LevelInfo
          }

  -- TODO: allow user to configure the index provider + hackage opts
  let hackageOptions = PackageIndex.defaultHackageOptions

  pure $ logOptions `HCons` hackageOptions `HCons` HNil
