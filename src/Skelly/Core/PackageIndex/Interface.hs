{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.PackageIndex.Interface (
  -- * Service
  Service (..),
  PackageIndex (..),
  PackageIndexCursor (..),
  PackageName,
  PackageInfo (..),

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
  { lookupPackageByName :: PackageName -> IO (Maybe PackageInfo)
  }

type PackageName = Text

data PackageInfo = PackageInfo
  { availableVersions :: [Version]
  , preferredVersionRange :: VersionRange
  }

getLatestVersion :: Service -> Text -> IO Version
getLatestVersion Service{..} package =
  withPackageIndex $ \index ->
    withCursor index $ \PackageIndexCursor{..} -> do
      PackageInfo{..} <-
        lookupPackageByName package >>= maybe (throwIO $ UnknownPackage package) pure

      case chooseBestVersion preferredVersionRange availableVersions of
        Nothing -> throwIO $ NoValidVersions package availableVersions preferredVersionRange
        Just version -> pure version
