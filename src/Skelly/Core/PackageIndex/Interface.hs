{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.PackageIndex.Interface (
  -- * Service
  Service (..),
  PackageIndex (..),
  PackageIndexCursor (..),
  PackageName,
  PackageVersions (..),

  -- * Methods
  getLatestVersion,
) where

import Data.Text (Text)
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Utils.Version (Version, VersionRange, chooseBestVersion)
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

      case chooseBestVersion preferredVersionRange availableVersions of
        Nothing -> throwIO $ NoValidVersions package availableVersions preferredVersionRange
        Just version -> pure version
