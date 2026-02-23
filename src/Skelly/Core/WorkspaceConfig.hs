{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.WorkspaceConfig (
  -- * Types
  WorkspaceConfig (..),
  PackageFlags,

  -- * Methods
  load,
) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.Utils.Config (discoverConfig)
import Skelly.Core.Utils.TOML qualified as TOML
import UnliftIO.Exception (fromEither, fromEitherM)

-- TODO: add a Service that loads + caches the workspace config?

-- | The `skelly.toml` file.
data WorkspaceConfig = WorkspaceConfig
  { packageFlags :: PackageFlags
  }

type PackageFlags = Map Text [(Text, Bool)]

load :: Logging.Service -> IO WorkspaceConfig
load loggingService = discoverConfig loggingService "skelly.toml" >>= loadFromFile

loadFromFile :: FilePath -> IO WorkspaceConfig
loadFromFile configPath = do
  rawConfig <- fromEitherM $ TOML.decodeFile configPath
  fromEither $ TOML.parseWith decodeConfig rawConfig

decodeConfig :: TOML.Decoder WorkspaceConfig
decodeConfig = do
  packageFlags <- fmap (map parseFlag) <$> TOML.getField "flags"
  pure WorkspaceConfig{..}
 where
  parseFlag flag =
    case Text.uncons flag of
      Just ('-', flag') -> (flag', False)
      _ -> (flag, True)
