{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.PackageConfig (
  -- * Types
  PackageConfig,
  LibraryInfo (..),
  BinaryInfo (..),
  SharedInfo (..),

  -- * Getters
  packageName,
  packageVersion,
  packageToolchainGHC,
  packageDependencies,
  packageLibraries,
  packageBinaries,
  allDependencies,

  -- * Methods
  load,
  save,
  addDependency,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.Types.PackageId (PackageName)
import Skelly.Core.Types.Version (
  Version,
  VersionRange (VersionRangeAnd),
  parseVersion,
  parseVersionRange,
  renderVersionRange,
 )
import Skelly.Core.Utils.Config (discoverConfig)
import Skelly.Core.Utils.TOML qualified as TOML
import UnliftIO.Exception (fromEither, fromEitherM)

-- | The `hspackage.toml` file.
data PackageConfig = PackageConfig
  { configPath :: FilePath
  , parsedConfig :: ParsedPackageConfig
  -- ^ Invariant: In sync with rawConfig
  , rawConfig :: TOML.Document
  -- ^ Invariant: In sync with parsedConfig
  }

data ParsedPackageConfig = ParsedPackageConfig
  { _packageName :: PackageName
  , _packageVersion :: Version
  , _packageToolchainGHC :: VersionRange
  , _packageDependencies :: DependencyMap
  , _packageLibraries :: Map PackageName LibraryInfo
  , _packageBinaries :: Map PackageName BinaryInfo
  }

type DependencyMap = Map PackageName VersionRange

-- TODO: add ghc options
-- TODO: add default extensions
data SharedInfo = SharedInfo
  { sourceDirs :: [FilePath]
  , dependencies :: DependencyMap
  }
  deriving (Show)

data LibraryInfo = LibraryInfo
  { sharedInfo :: SharedInfo
  }
  deriving (Show)

data BinaryInfo = BinaryInfo
  { sharedInfo :: SharedInfo
  , mainFile :: FilePath
  }
  deriving (Show)

load :: Logging.Service -> IO PackageConfig
load loggingService =
  discoverConfig loggingService "hspackage.toml" >>= loadFromFile

loadFromFile :: FilePath -> IO PackageConfig
loadFromFile configPath = do
  rawConfig <- fromEitherM $ TOML.decodeFile configPath
  parsedConfig <- fromEither $ TOML.parseWith decodeParsedConfig rawConfig
  pure PackageConfig{..}

decodeParsedConfig :: TOML.Decoder ParsedPackageConfig
decodeParsedConfig = do
  packageDeps <- TOML.getFieldsWith decodeDependencies ["skelly", "dependencies"]
  pure ParsedPackageConfig
    <*> TOML.getFields ["skelly", "package", "name"]
    <*> TOML.getFieldsWith decodeVersion ["skelly", "package", "version"]
    <*> TOML.getFieldsWith decodeVersionRange ["skelly", "toolchain", "ghc"]
    <*> pure packageDeps
    <*> decodeLibraries packageDeps
    <*> decodeBinaries packageDeps
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

    decodeDependencies :: TOML.Decoder DependencyMap
    decodeDependencies = TOML.makeDecoder $ \case
      TOML.Table table -> traverse (TOML.runDecoder decodeVersionRange) table
      v -> TOML.typeMismatch v

    -- TODO: parse from [[skelly.lib]]
    -- TODO: get deps in [skelly.lib.dependencies] if present
    -- TODO: default name: same name as package
    -- TODO: error if duplicate names
    decodeLibraries :: DependencyMap -> TOML.Decoder (Map PackageName LibraryInfo)
    decodeLibraries deps =
      pure . Map.singleton "skelly" $
        LibraryInfo
          { sharedInfo =
              SharedInfo
                { sourceDirs = ["src"]
                , dependencies = deps
                }
          }

    -- TODO: parse from [[skelly.bin]]
    -- TODO: default name: same name as package
    -- TODO: default mainFile:
    --         if one bin => src/Main.hs
    --         if directory exists => src/bin/<name>/Main.hs
    --         otherwise => src/bin/<name>.hs
    -- TODO: default sourceDirs:
    --         if directory exists => [src/bin/<name>/]
    --         otherwise => []
    -- TODO: error if duplicate names
    decodeBinaries :: DependencyMap -> TOML.Decoder (Map PackageName BinaryInfo)
    decodeBinaries deps =
      pure . Map.singleton "skelly" $
        BinaryInfo
          { sharedInfo =
              SharedInfo
                { sourceDirs = []
                , dependencies = deps
                }
          , mainFile = "src/Main.hs"
          }

{----- Getters -----}

getParsedField :: (ParsedPackageConfig -> a) -> PackageConfig -> a
getParsedField f PackageConfig{parsedConfig} = f parsedConfig

packageName :: PackageConfig -> PackageName
packageName = getParsedField $ \ParsedPackageConfig{_packageName = x} -> x

packageVersion :: PackageConfig -> Version
packageVersion = getParsedField $ \ParsedPackageConfig{_packageVersion = x} -> x

packageToolchainGHC :: PackageConfig -> VersionRange
packageToolchainGHC = getParsedField $ \ParsedPackageConfig{_packageToolchainGHC = x} -> x

packageDependencies :: PackageConfig -> DependencyMap
packageDependencies = getParsedField $ \ParsedPackageConfig{_packageDependencies = x} -> x

packageLibraries :: PackageConfig -> Map PackageName LibraryInfo
packageLibraries = getParsedField $ \ParsedPackageConfig{_packageLibraries = x} -> x

packageBinaries :: PackageConfig -> Map PackageName BinaryInfo
packageBinaries = getParsedField $ \ParsedPackageConfig{_packageBinaries = x} -> x

allDependencies :: PackageConfig -> DependencyMap
allDependencies config =
  Map.unionsWith VersionRangeAnd . concat $
    [ [ dependencies | LibraryInfo{sharedInfo = SharedInfo{dependencies}} <- Map.elems $ packageLibraries config ]
    , [ dependencies | BinaryInfo{sharedInfo = SharedInfo{dependencies}} <- Map.elems $ packageBinaries config ]
    ]

{----- Updaters -----}

save :: PackageConfig -> IO ()
save PackageConfig{..} = TOML.encodeFile configPath rawConfig

addDependency :: PackageName -> VersionRange -> PackageConfig -> PackageConfig
addDependency dep versionRange config@PackageConfig{..} =
  config
    { parsedConfig =
        parsedConfig
          { _packageDependencies = Map.insert dep versionRange _packageDependencies
          }
    , rawConfig =
        TOML.setKey
          ["skelly", "dependencies", dep]
          (TOML.String $ renderVersionRange versionRange)
          rawConfig
    }
  where
    ParsedPackageConfig{..} = parsedConfig
