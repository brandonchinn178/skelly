{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.CommandBuild (commandBuild) where

import Options.Applicative
import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI
import Skelly.Core.Build qualified as Build
import Skelly.Core.Solver qualified as Solver

commandBuild :: Command
commandBuild =
  Command
    { cmdName = "build"
    , cmdDesc = "Build a Haskell project"
    , cmdParse =
        Build.Options
          <$> targetsParser "lib" "library" "libraries"
          <*> targetsParser "bin" "binary" "binaries"
          <*> targetsParser "test" "test target" "tests"
    , cmdExec = execute
    }
  where
    targetsParser flagName labelOne labelMany =
      asum
        [ flag' Build.AllTargets . mconcat $
          [ long (flagName <> "s")
          , help $ "Build all " <> labelMany
          ]
        , fmap Build.Targets . many . strOption . mconcat $
          [ long flagName
          , metavar "name"
          , help $ "Build the specified " <> labelOne
          ]
        ]

execute :: CLI.Service -> Build.Options -> IO ()
execute CLI.Service{..} = Build.run service . resolveOpts
  where
    solverService = Solver.initService packageIndexService
    service = Build.initService loggingService packageIndexService solverService

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
