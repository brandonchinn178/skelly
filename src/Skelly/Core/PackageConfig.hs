{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Skelly.Core.PackageConfig (
  -- * PackageConfig
  PackageConfig (..),
  LibraryInfo (..),
  BinaryInfo (..),
  SharedInfo (..),

  -- ** Methods
  load,
  modify,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import KDL qualified
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.Types.PackageId (PackageName)
import Skelly.Core.Types.Version (
  Version,
  VersionRange (VersionRangeAnd),
  parseVersion,
  parseVersionRange,
 )
import Skelly.Core.Utils.Config (findConfig)
import Skelly.Core.Utils.KDL qualified as KDL
import UnliftIO.Exception (throwIO)

-- | The `hspackage.toml` file.
data PackageConfig = PackageConfig
  { packageName :: PackageName
  , packageVersion :: Version
  , packageToolchainGHC :: VersionRange
  , packageDependencies :: DependencyMap
  , packageLibraries :: Map PackageName LibraryInfo
  , packageBinaries :: Map PackageName BinaryInfo
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

instance HasField "allDependencies" PackageConfig DependencyMap where
  getField config =
    Map.unionsWith VersionRangeAnd . concat $
      [ [dependencies | LibraryInfo{sharedInfo = SharedInfo{dependencies}} <- Map.elems $ config.packageLibraries]
      , [dependencies | BinaryInfo{sharedInfo = SharedInfo{dependencies}} <- Map.elems $ config.packageBinaries]
      ]

{----- Load a PackageConfig -----}

load :: Logging.Service -> IO PackageConfig
load loggingService = findPackageConfig loggingService >>= loadFromFile

findPackageConfig :: Logging.Service -> IO FilePath
findPackageConfig loggingService = findConfig loggingService "hspackage.kdl"

loadFromFile :: FilePath -> IO PackageConfig
loadFromFile path =
  KDL.decodeFileWith decoder path >>= either (throwIO . InvalidConfig path . KDL.renderDecodeError) pure

decoder :: KDL.DocumentDecoder PackageConfig
decoder = KDL.document $ do
  KDL.nodeWith "package" $ do
    packageName <- KDL.children $ KDL.argAt "name"
    packageVersion <- KDL.children $ KDL.argAt "version"
    packageToolchainGHC <- KDL.children . KDL.nodeWith "toolchain" . KDL.children $ KDL.argAt "ghc"
    packageDependencies <- KDL.children . KDL.nodeWith "dependencies" . KDL.children $ KDL.remainingUniqueNodesWith KDL.arg
    packageLibraries <- decodeLibraries packageDependencies
    packageBinaries <- decodeBinaries packageDependencies
    pure PackageConfig{..}
 where
  -- TODO: parse from [[skelly.lib]]
  -- TODO: get deps in [skelly.lib.dependencies] if present
  -- TODO: default name: same name as package
  -- TODO: error if duplicate names
  decodeLibraries :: DependencyMap -> KDL.NodeDecoder (Map PackageName LibraryInfo)
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
  decodeBinaries :: DependencyMap -> KDL.NodeDecoder (Map PackageName BinaryInfo)
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

instance KDL.DecodeValue Version where
  valueDecoder = KDL.withDecoder KDL.text $ \s ->
    maybe (KDL.failM $ "Invalid version: " <> s) pure $
      parseVersion s

instance KDL.DecodeValue VersionRange where
  valueDecoder = KDL.withDecoder KDL.text $ \s ->
    maybe (KDL.failM $ "Invalid version range: " <> s) pure $
      parseVersionRange s

{----- Modify a config -----}

modify :: Logging.Service -> (KDL.Document -> KDL.Document) -> IO ()
modify loggingService f = do
  fp <- findPackageConfig loggingService
  doc <- KDL.parseFile fp >>= either (throwIO . InvalidConfig fp) pure
  let doc' = f doc
  Text.writeFile fp $ KDL.render doc'
