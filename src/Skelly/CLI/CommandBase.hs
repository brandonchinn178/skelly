{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Skelly.CLI.CommandBase (
  BaseOptions,
  parseBaseOptions,
) where

import Data.HList (HList (..))
import Options.Applicative qualified as Opt
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageIndex qualified as PackageIndex

type BaseOptions = HList '[Logging.Options, PackageIndex.HackageOptions]

parseBaseOptions :: Opt.Parser BaseOptions
parseBaseOptions = do
  optionVerbose <-
    Opt.switch . mconcat $
      [ Opt.long "verbose"
      , Opt.short 'v'
      , Opt.help "Output more verbose logs"
      ]
  pure $
    let
      logOptions =
        Logging.Options
          { logLevel = if optionVerbose then Logging.LevelDebug else Logging.LevelInfo
          }

      -- TODO: allow user to configure the index provider + hackage opts
      packageIndexOptions = PackageIndex.defaultHackageOptions
     in
      logOptions `HCons` packageIndexOptions `HCons` HNil
