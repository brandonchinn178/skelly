{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Config (
  PackageConfig,
  packageName,
  packageVersion,
  packageToolchainGHC,
  packageDependencies,

  -- * Methods
  loadConfig,
  addDependency,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Skelly.Core.Utils.TOML qualified as TOML
import Skelly.Core.Utils.Version (
  Version,
  VersionRange,
  parseVersion,
  parseVersionRange,
  renderVersionRange,
 )
import UnliftIO.Exception (fromEither, fromEitherM)

-- | The `hspackage.toml` file.
--
-- Invariant: parsedConfig and rawConfig are in sync.
data PackageConfig = PackageConfig
  { parsedConfig :: ParsedPackageConfig
  , rawConfig :: TOML.Document
  }

data ParsedPackageConfig = ParsedPackageConfig
  { _packageName :: Text
  , _packageVersion :: Version
  , _packageToolchainGHC :: VersionRange
  , _packageDependencies :: Map Text VersionRange
  }

loadConfig :: FilePath -> IO PackageConfig
loadConfig fp = do
  rawConfig <- fromEitherM $ TOML.decodeFile fp
  parsedConfig <- fromEither $ TOML.parseWith decodeParsedConfig rawConfig
  pure PackageConfig{..}

decodeParsedConfig :: TOML.Decoder ParsedPackageConfig
decodeParsedConfig =
  pure ParsedPackageConfig
    <*> TOML.getFields ["skelly", "package", "name"]
    <*> TOML.getFieldsWith decodeVersion ["skelly", "package", "version"]
    <*> TOML.getFieldsWith decodeVersionRange ["skelly", "toolchain", "ghc"]
    <*> TOML.getFieldsWith decodeDependencies ["skelly", "dependencies"]
  where
    decodeVersion :: TOML.Decoder Version
    decodeVersion = TOML.makeDecoder $ \v ->
      case v of
        TOML.String s ->
          case parseVersion s of
            Just version -> pure version
            Nothing -> TOML.invalidValue "Invalid version" v
        _ -> TOML.typeMismatch v

    decodeVersionRange :: TOML.Decoder VersionRange
    decodeVersionRange = TOML.makeDecoder $ \v ->
      case v of
        TOML.String s ->
          case parseVersionRange s of
            Just versionRange -> pure versionRange
            Nothing -> TOML.invalidValue "Invalid version range" v
        _ -> TOML.typeMismatch v

    decodeDependencies :: TOML.Decoder (Map Text VersionRange)
    decodeDependencies = TOML.makeDecoder $ \case
      TOML.Table table -> traverse (TOML.runDecoder decodeVersionRange) table
      v -> TOML.typeMismatch v

{----- Getters -----}

getParsedField :: (ParsedPackageConfig -> a) -> PackageConfig -> a
getParsedField f PackageConfig{parsedConfig} = f parsedConfig

packageName :: PackageConfig -> Text
packageName = getParsedField $ \ParsedPackageConfig{_packageName = x} -> x

packageVersion :: PackageConfig -> Version
packageVersion = getParsedField $ \ParsedPackageConfig{_packageVersion = x} -> x

packageToolchainGHC :: PackageConfig -> VersionRange
packageToolchainGHC = getParsedField $ \ParsedPackageConfig{_packageToolchainGHC = x} -> x

packageDependencies :: PackageConfig -> Map Text VersionRange
packageDependencies = getParsedField $ \ParsedPackageConfig{_packageDependencies = x} -> x

{----- Updaters -----}

addDependency :: Text -> VersionRange -> PackageConfig -> PackageConfig
addDependency dep versionRange PackageConfig{..} =
  PackageConfig
    { parsedConfig =
        parsedConfig
          { _packageDependencies = Map.insert dep versionRange _packageDependencies
          }
    , rawConfig =
        TOML.setKey
          ["dependencies", dep]
          (TOML.String $ renderVersionRange versionRange)
          rawConfig
    }
  where
    ParsedPackageConfig{..} = parsedConfig
