{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.PackageIndex.Interface (
  -- * Service
  Service (..),
  PackageIndex (..),
  PackageIndexCursor (..),
  PackageInfo (..),
  PackageVersionInfo (..),

  -- * Methods
  withCursor,
  getPackageVersionInfo,
  getPackageInfo,

  -- * Helpers
  getLatestVersion,
) where

import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Utils.Cabal (PackageInfo (..))
import Skelly.Core.Utils.PackageId (PackageId, PackageName)
import Skelly.Core.Utils.Version (Version, VersionRange, chooseBestVersion)
import UnliftIO.Exception (throwIO)

data Service = Service
  { withPackageIndex :: forall a. (PackageIndex -> IO a) -> IO a
  }

data PackageIndex = PackageIndex
  { withIndexCursor :: forall a. (PackageIndexCursor -> IO a) -> IO a
  , updateMetadata :: IO ()
  , -- | Download the given package to the given directory
    --
    -- e.g. The following call creates the following directory tree:
    --
    -- >>> downloadPackageTo service (PackageId "foo" "0.0.0") "/tmp/packages/"
    --
    -- @
    -- /tmp/packages/
    -- `-- foo-0.0.0.tar.gz
    -- `-- foo-0.0.0/
    --     `-- foo.cabal
    --     `-- Foo.hs
    -- @
    downloadPackage :: PackageId -> FilePath -> IO ()
  }

data PackageIndexCursor = PackageIndexCursor
  { lookupPackageVersionInfo :: PackageName -> IO (Maybe PackageVersionInfo)
  , lookupPackageInfo :: PackageId -> IO (Maybe PackageInfo)
  }

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
  case chooseBestVersion preferredVersionRange availableVersions of
    Nothing -> throwIO $ NoValidVersions package availableVersions preferredVersionRange
    Just version -> pure version
