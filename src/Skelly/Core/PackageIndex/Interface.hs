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
  withCursor,
  getPackageVersionInfo,
  getPackageInfo,

  -- * Helpers
  getLatestVersion,
) where

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
  { withIndexCursor :: forall a. (PackageIndexCursor -> IO a) -> IO a
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

withCursor :: Service -> (PackageIndexCursor -> IO a) -> IO a
withCursor service f = withPackageIndex service $ \index -> withIndexCursor index f

getPackageVersionInfo :: PackageIndexCursor -> PackageName -> IO PackageVersionInfo
getPackageVersionInfo PackageIndexCursor{..} package =
  lookupPackageVersionInfo package >>= maybe (throwIO $ UnknownPackage package) pure

getPackageInfo :: PackageIndexCursor -> PackageId -> IO PackageInfo
getPackageInfo PackageIndexCursor{..} packageId =
  lookupPackageInfo packageId >>= maybe (throwIO $ PackageIdNotFound packageId) pure

{----- Helpers -----}

getLatestVersion :: PackageIndexCursor -> PackageName -> IO Version
getLatestVersion cursor package = do
  PackageVersionInfo{..} <- getPackageVersionInfo cursor package
  case chooseBestVersion (compileRange preferredVersionRange) availableVersions of
    Nothing -> throwIO $ NoValidVersions package availableVersions preferredVersionRange
    Just version -> pure version
