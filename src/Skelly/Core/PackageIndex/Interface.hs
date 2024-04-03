{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.PackageIndex.Interface (
  -- * Service
  Service (..),
  PackageIndex (..),
  PackageIndexCursor (..),
  PackageInfo (..),
  PackageName,
  PackageVersionInfo (..),

  -- * Methods
  getLatestVersion,
  getPackageVersions,
  getPackageDeps,
) where

import Data.Map (Map)
import Data.Text (Text)
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Utils.Cabal (PackageInfo (..))
import Skelly.Core.Utils.PackageId (PackageId)
import Skelly.Core.Utils.Version (Version, VersionRange, chooseBestVersion, compileRange)
import UnliftIO.Exception (throwIO)

data Service = Service
  { withPackageIndex :: forall a. (PackageIndex -> IO a) -> IO a
  }

data PackageIndex = PackageIndex
  { withCursor :: forall a. (PackageIndexCursor -> IO a) -> IO a
  , updateMetadata :: IO ()
  }

data PackageIndexCursor = PackageIndexCursor
  { lookupPackageVersionInfo :: PackageName -> IO (Maybe PackageVersionInfo)
  , lookupPackageInfo :: PackageId -> IO (Maybe PackageInfo)
  }

type PackageName = Text

data PackageVersionInfo = PackageVersionInfo
  { availableVersions :: [Version]
  , preferredVersionRange :: VersionRange
  }

getLatestVersion :: Service -> PackageName -> IO Version
getLatestVersion service package = do
  PackageVersionInfo{..} <- getPackageVersionInfo service package
  case chooseBestVersion (compileRange preferredVersionRange) availableVersions of
    Nothing -> throwIO $ NoValidVersions package availableVersions preferredVersionRange
    Just version -> pure version

getPackageVersions :: Service -> PackageName -> IO [Version]
getPackageVersions service package = availableVersions <$> getPackageVersionInfo service package

getPackageVersionInfo :: Service -> PackageName -> IO PackageVersionInfo
getPackageVersionInfo Service{..} package =
  withPackageIndex $ \index ->
    withCursor index $ \PackageIndexCursor{..} ->
      lookupPackageVersionInfo package >>= maybe (throwIO $ UnknownPackage package) pure

getPackageDeps :: Service -> PackageId -> IO (Map PackageName VersionRange)
getPackageDeps Service{..} packageId =
  withPackageIndex $ \index ->
    withCursor index $ \PackageIndexCursor{..} ->
      lookupPackageInfo packageId >>= \case
        Nothing -> throwIO $ PackageIdNotFound packageId
        Just info -> pure $ packageDependencies info
