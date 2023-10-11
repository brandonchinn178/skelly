{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.CommandBuild (commandBuild) where

import Data.Text (Text)
import Options.Applicative
import Skelly.CLI.Command

commandBuild :: Command
commandBuild =
  Command
    { cmdName = "build"
    , cmdDesc = "Build a Haskell project"
    , cmdParse =
        BuildOptions
          <$> pure [] -- TODO
    , cmdRun = run
    }

data BuildOptions = BuildOptions
  { buildTargets :: [Text]
  }

run :: BuildOptions -> SharedOptions -> IO ()
run _ _ = putStrLn "TODO: build"
