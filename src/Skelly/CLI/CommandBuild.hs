{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.CommandBuild (commandBuild) where

import Data.Text (Text)
import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI

commandBuild :: Command
commandBuild =
  Command
    { cmdName = "build"
    , cmdDesc = "Build a Haskell project"
    , cmdParse =
        BuildOptions
          <$> pure [] -- TODO
    , cmdExec = execute
    }

data BuildOptions = BuildOptions
  { buildTargets :: [Text]
  }

execute :: CLI.Service -> BuildOptions -> IO ()
execute _ BuildOptions{..} = putStrLn $ "TODO: build: " ++ show buildTargets
