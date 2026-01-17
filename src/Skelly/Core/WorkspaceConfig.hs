{-# LANGUAGE LambdaCase #-}
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
import KDL qualified
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.Utils.Config (findConfig)
import Skelly.Core.Utils.KDL qualified as KDL
import UnliftIO.Exception (throwIO)

-- TODO: add a Service that loads + caches the workspace config?

-- | The `skelly.toml` file.
data WorkspaceConfig = WorkspaceConfig
  { packageFlags :: PackageFlags
  }

type PackageFlags = Map Text [(Text, Bool)]

load :: Logging.Service -> IO WorkspaceConfig
load loggingService = findConfig loggingService "skelly.toml" >>= loadFromFile

loadFromFile :: FilePath -> IO WorkspaceConfig
loadFromFile configPath =
  KDL.decodeFileWith decoder configPath
    >>= either (throwIO . InvalidConfig configPath . KDL.renderDecodeError) pure

decoder :: KDL.DocumentDecoder WorkspaceConfig
decoder = KDL.document $ do
  packageFlags <- KDL.nodeWith "flags" . KDL.children $ KDL.remainingUniqueNodesWith decodeFlag
  pure WorkspaceConfig{..}

decodeFlag :: KDL.NodeDecoder [(Text, Bool)]
decodeFlag = KDL.many . KDL.argWith $ parseFlag <$> KDL.text
 where
  parseFlag flag =
    case Text.uncons flag of
      Just ('-', flag') -> (flag', False)
      _ -> (flag, True)
