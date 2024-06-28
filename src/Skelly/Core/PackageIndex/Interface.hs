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
  PackageVersions (..),

  -- * Methods
  getLatestVersion,
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
  { lookupPackageVersions :: PackageName -> IO (Maybe PackageVersions)
  , lookupPackageInfo :: PackageId -> IO (Maybe PackageInfo)
  }

type PackageName = Text

data PackageVersions = PackageVersions
  { availableVersions :: [Version]
  , preferredVersionRange :: VersionRange
  }

getLatestVersion :: Service -> Text -> IO Version
getLatestVersion Service{..} package =
  withPackageIndex $ \index ->
    withCursor index $ \PackageIndexCursor{..} -> do
      PackageVersions{..} <-
        lookupPackageVersions package >>= maybe (throwIO $ UnknownPackage package) pure

      case chooseBestVersion (compileRange preferredVersionRange) availableVersions of
        Nothing -> throwIO $ NoValidVersions package availableVersions preferredVersionRange
        Just version -> pure version

getPackageDeps :: Service -> PackageId -> IO (Map PackageName VersionRange)
getPackageDeps Service{..} packageId =
  withPackageIndex $ \index ->
    withCursor index $ \PackageIndexCursor{..} ->
      lookupPackageInfo packageId >>= \case
        Nothing -> throwIO $ PackageIdNotFound packageId
        Just info -> pure $ packageDependencies info
