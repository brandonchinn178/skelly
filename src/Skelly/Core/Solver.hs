{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Solver (
  Service (..),
  initService,
  PackageDeps,
  run,
) where

import Control.Applicative (asum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Maybe qualified as MaybeT
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as Strict (Map)
import Data.Map.Strict qualified as Map.Strict
import Data.Map.Merge.Strict qualified as Map.Strict
import Data.Text (Text)
import Skelly.Core.Error (SkellyError (DependencyResolutionFailure))
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Utils.PackageId (PackageId (..))
import Skelly.Core.Utils.Version (
  CompiledVersionRange,
  Version,
  VersionRange,
  getVersionPreferences,
  compileRange,
  intersectRange,
  isSingletonRange,
  singletonRange,
 )
import UnliftIO.Exception (throwIO)

type PackageName = Text
type PackageDeps = Map PackageName VersionRange

data Service = Service
  { withCursor :: forall a. (PackageIndex.PackageIndexCursor -> IO a) -> IO a
  , getPackageDeps :: PackageIndex.PackageIndexCursor -> PackageId -> IO PackageDeps
  , getPackageVersions :: PackageIndex.PackageIndexCursor -> PackageName -> IO [Version]
  }

initService :: PackageIndex.Service -> Service
initService pkgIndexService =
  Service
    { withCursor = PackageIndex.withCursor pkgIndexService
    , getPackageDeps = \cursor -> fmap PackageIndex.packageDependencies . PackageIndex.getPackageInfo cursor
    , getPackageVersions = \cursor -> fmap PackageIndex.availableVersions . PackageIndex.getPackageVersionInfo cursor
    }

-- | Get the list of transitive dependencies in topological order.
run :: Service -> PackageDeps -> IO [PackageId]
run Service{..} deps0 =
  withCursor $ \cursor ->
    runMaybeT (compileDeps deps0 >>= \deps0' -> go cursor deps0' (Map.keys deps0))
      >>= maybe (throwIO DependencyResolutionFailure) pure
  where
    compileDeps = fmap (Map.Strict.fromAscList . Map.toAscList) . traverse (MaybeT.hoistMaybe . compileRange)

    -- TODO: restrict builtin packages like base?
    go ::
      PackageIndex.PackageIndexCursor
      -> Strict.Map PackageName CompiledVersionRange -- ^ The dependency constraints so far
      -> [PackageName] -- ^ The queue of package names
      -> MaybeT IO [PackageId] -- ^ Nothing if conflict
    go _ _ [] = pure []
    go cursor deps (pkgName : rest) =
      case Map.Strict.lookup pkgName deps of
        -- package is in the queue without registering a range; this is a bug
        Nothing -> error $ "Unexpectedly failed to find package " <> show pkgName <> ":\n" <> show deps
        Just range
          -- package already resolved
          | isSingletonRange range -> go cursor deps rest
          -- package not resolved
          | otherwise-> do
              versions <- liftIO $ getVersionPreferences range <$> getPackageVersions cursor pkgName
              -- try each version, going to the next version if fail
              -- TODO: implement cabal's quick-abort "has the same characteristics that caused the previous version to fail"
              asum
                [ do
                    -- TODO: logging
                    -- get package dependencies
                    pkgDeps <- liftIO (getPackageDeps cursor pkgId) >>= compileDeps
                    -- update constraints, failing if any conflicts are found
                    deps' <-
                      Map.Strict.mergeA
                        Map.Strict.preserveMissing
                        Map.Strict.preserveMissing
                        (Map.Strict.zipWithAMatched $ \_ r1 r2 -> MaybeT.hoistMaybe $ intersectRange r1 r2)
                        deps
                        pkgDeps
                    -- set version for current package and recurse
                    (pkgId :) <$> go cursor (Map.Strict.insert pkgName (singletonRange pkgVer) deps') rest
                | pkgVer <- versions
                , let pkgId = PackageId pkgName pkgVer
                ]
