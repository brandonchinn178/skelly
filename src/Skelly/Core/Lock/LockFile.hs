{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Skelly.Core.Lock.LockFile (
  LockFile (..),
  LockFilePackageInfo (..),
  LockFileDepInfo (..),
  readLockFile,
  writeLockFile,

  -- * Low-level API
  decodeLockFile,
  encodeLockFile,
) where

import Control.Monad (guard)
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash qualified as Crypto
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Types.PackageId (PackageName)
import Skelly.Core.Types.Version (Version, CompiledVersionRange)
import UnliftIO.Exception (throwIO)

data LockFile = LockFile
  { ghcVersion :: Version
  , packages :: Map PackageName LockFilePackageInfo
    -- ^ The packages in the workspace and their dependencies
  , dependencies :: Map PackageName LockFileDepInfo
    -- ^ The resolved information of all transitive dependencies
  }
  deriving (Show, Eq)

data LockFilePackageInfo = LockFilePackageInfo
  { deps :: Map PackageName CompiledVersionRange
  }
  deriving (Show, Eq)

data LockFileDepInfo = LockFileDepInfo
  { version :: Version
  , integrity :: Digest SHA256
  , deps :: Map PackageName CompiledVersionRange
  }
  deriving (Show, Eq)

data LockFileVersion
  = LOCK_FILE_VERSION_0
  deriving (Show, Eq, Enum, Bounded)

instance Aeson.FromJSON LockFileVersion where
  parseJSON v = do
    x <- Aeson.parseJSON v
    guard $ 0 <= x && x <= fromEnum (maxBound :: LockFileVersion)
    pure $ toEnum x

instance Aeson.ToJSON LockFileVersion where
  toJSON = Aeson.toJSON . fromEnum

instance Aeson.FromJSON LockFile where
  parseJSON = Aeson.withObject "LockFile" $ \o -> do
    o .: "version" >>= \case
      LOCK_FILE_VERSION_0 -> do
        ghcVersion <- o .: "ghc-version"
        packages <- o .: "packages"
        dependencies <- o .: "dependencies"
        pure LockFile{..}

instance Aeson.FromJSON LockFilePackageInfo where
  parseJSON = Aeson.withObject "LockFilePackageInfo" $ \o -> do
    deps <- o .: "dependencies"
    pure LockFilePackageInfo{..}

instance Aeson.FromJSON LockFileDepInfo where
  parseJSON = Aeson.withObject "LockFileDepInfo" $ \o -> do
    version <- o .: "version"
    Just integrity <- decodeDigest <$> o .: "integrity"
    deps <- o .: "dependencies"
    pure LockFileDepInfo{..}

readLockFile :: FilePath -> IO LockFile
readLockFile fp = do
  lockFileContent <- Text.readFile fp
  maybe (throwIO InvalidLockFile) pure $ decodeLockFile lockFileContent

writeLockFile :: FilePath -> LockFile -> IO ()
writeLockFile fp lockFile = Text.writeFile fp $ encodeLockFile lockFile <> "\n"

-- TODO: resolve merge conflicts
decodeLockFile :: Text -> Maybe LockFile
decodeLockFile = Aeson.decode . TextL.encodeUtf8 . TextL.fromStrict

-- | Encode lock file as a JSON string.
--
-- Manually rendering it ourselves to ensure we have control over the formatting
-- (Aeson doesn't guarantee ordering of keys)
encodeLockFile :: LockFile -> Text
encodeLockFile LockFile{..} =
  render 0 $
    JsonObject
      [ ("version", encode LOCK_FILE_VERSION_0)
      , ("ghc-version", encode ghcVersion)
      , ("packages", encodeMap encodeLockFilePackageInfo packages)
      , ("dependencies", encodeMap encodeLockFileDepInfo dependencies)
      ]
  where
    encode :: Aeson.ToJSON a => a -> Json
    encode = JsonRaw . TextL.toStrict . TextL.decodeUtf8 . Aeson.encode

    encodeMap encodeVal m = JsonObject [(k, encodeVal v) | (k, v) <- Map.toAscList m]
    encodeLockFilePackageInfo LockFilePackageInfo{..} =
      JsonObject
        [ ("dependencies", encodeMap encode deps)
        ]
    encodeLockFileDepInfo LockFileDepInfo{..} =
      JsonObject
        [ ("version", encode version)
        , ("integrity", encode $ encodeDigest integrity)
        , ("dependencies", encodeMap encode deps)
        ]

    indent lvl = Text.replicate (lvl * 4) " "
    render lvl = \case
      JsonObject kvs ->
        Text.concat
          [ "{"
          , Text.intercalate "," $
              [ "\n" <> indent (lvl + 1) <> "\"" <> k <> "\": " <> render (lvl + 1) v
              | (k, v) <- kvs
              ]
          , "\n" <> indent lvl <> "}"
          ]
      JsonRaw s -> s

data Json = JsonObject [(Text, Json)] | JsonRaw Text

{----- Serializing Digest -----}

encodeDigest :: Digest SHA256 -> Text
encodeDigest digest = "sha256=" <> (Text.pack . show) digest

decodeDigest :: Text -> Maybe (Digest SHA256)
decodeDigest input = do
  digest <- Text.stripPrefix "sha256=" input
  Crypto.digestFromByteString $ Text.encodeUtf8 digest
