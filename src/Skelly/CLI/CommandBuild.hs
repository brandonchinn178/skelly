{-# LANGUAGE DataKinds #-}

module Skelly.CLI.CommandBuild (commandBuild) where

import Options.Applicative qualified as Opt
import Skelly.CLI.Command
import Skelly.CLI.CommandBase (BaseOptions)
import Skelly.Core.Build qualified as Build

commandBuild :: CommandSpec '[BaseOptions]
commandBuild =
  CommandSpec
    { cmdName = "build"
    , cmdDesc = "Build a Haskell project"
    , cmdImpl = execute
    , cmdOptions =
        Build.Options
          <$> targetsParser "lib" "library" "libraries"
          <*> targetsParser "bin" "binary" "binaries"
          <*> targetsParser "test" "test target" "tests"
    }
  where
    targetsParser flagName labelOne labelMany =
      Opt.asum
        [ Opt.flag' Build.AllTargets . mconcat $
          [ Opt.long (flagName <> "s")
          , Opt.help $ "Build all " <> labelMany
          ]
        , fmap Build.Targets . Opt.many . Opt.strOption . mconcat $
          [ Opt.long flagName
          , Opt.metavar "name"
          , Opt.help $ "Build the specified " <> labelOne
          ]
        ]

execute :: Build.Service -> Build.Options -> IO ()
execute service = Build.run service . resolveOpts
  where
    resolveOpts = resolveTargets

    -- If no targets specified, build all libs + bins
    resolveTargets opts =
      if all (== Build.Targets []) (Build.allOptionTargets opts)
        then
          opts
            { Build.libTargets = Build.AllTargets
            , Build.binTargets = Build.AllTargets
            }
        else
          opts
