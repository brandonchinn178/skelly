{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{- |
This module contains the dependency resolution solver.

For now, uses a fairly naive DFS with non-chronological backtracking.

References:
* https://en.wikipedia.org/wiki/Conflict-driven_clause_learning
* https://www.well-typed.com/blog/2015/03/qualified-goals/
-}
module Skelly.Core.Solver (
  Service (..),
  initService,
  SolvedPackage (..),
  run,

  -- * Helpers
  sortVersions,
  rankPackageDefault,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Validate (ValidateT, runValidateT)
import Control.Monad.Validate qualified as ValidateT
import Data.Bifunctor (first)
import Data.Cache.LRU (LRU)
import Data.Cache.LRU qualified as LRU
import Data.Graph qualified as Graph
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as HashPSQ
import Data.List (elemIndex)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as Strict (Map)
import Data.Map.Strict qualified as Map.Strict
import Data.Map.Merge.Strict qualified as Map.Strict
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Skelly.Core.CompilerEnv (CompilerEnv)
import Skelly.Core.CompilerEnv qualified as CompilerEnv
import Skelly.Core.Error (
  SkellyError (DependencyResolutionFailure),
 )
import Skelly.Core.Logging (logDebug)
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Service (IsService (..), loadService)
import Skelly.Core.Types.PackageId (
  PackageId (..),
  PackageName,
  renderPackageId,
 )
import Skelly.Core.Types.Version (
  CompiledVersionRange,
  Version,
  VersionRange,
  compileRange,
  inRange,
  intersectRange,
  isSingletonRange,
  prettyCompiledRange,
  singletonRange,
 )
import UnliftIO.MVar (MVar, newMVar, modifyMVar)
import UnliftIO.Exception (throwIO)

type RawPackageDeps = Map PackageName VersionRange
type PackageDeps = Map PackageName CompiledVersionRange

data Service = Service
  { loggingService :: Logging.Service
  , withCursor :: forall a. (PackageIndex.PackageIndexCursor -> IO a) -> IO a
  , getPackageDeps :: PackageIndex.PackageIndexCursor -> PackageId -> IO RawPackageDeps
  , getPackageVersionInfo :: PackageIndex.PackageIndexCursor -> PackageName -> IO PackageIndex.PackageVersionInfo
  , -- | Rank package, where higher is better.
    rankPackage :: PackageName -> Int
  }

instance
  ( IsService opts Logging.Service
  , IsService opts PackageIndex.Service
  ) =>
  IsService opts Service where
  initService = do
    loggingService <- loadService
    pkgIndexService <- loadService
    pure
      Service
        { loggingService
        , withCursor = PackageIndex.withCursor pkgIndexService
        , getPackageDeps = \cursor -> fmap PackageIndex.packageDependencies . PackageIndex.getPackageInfo cursor
        , getPackageVersionInfo = PackageIndex.getPackageVersionInfo
        , rankPackage = rankPackageDefault
        }

data SolvedPackage = SolvedPackage
  { packageId :: PackageId
  , packageDeps :: PackageDeps
  }

-- | Get the list of transitive dependencies in topological order, where
-- packages may not depend on any packages later in the list.
run :: Service -> CompilerEnv -> PackageDeps -> IO [SolvedPackage]
run service@Service{..} env initialDeps =
  withCursor $ \cursor -> do
    packageCache <- initPackageCache (getPackageVersionInfo cursor) (getPackageDeps cursor)
    runValidateT (runSolver service env packageCache initialDeps) >>= \case
      Right packages -> sortTopological packageCache packages
      Left _ -> throwIO DependencyResolutionFailure

type ConflictSet = Set PackageName
type PackageQueue = HashPSQ PackageName Int ()

runSolver ::
  Service
  -> CompilerEnv
  -> PackageCache
  -> PackageDeps
  -> ValidateT ConflictSet IO [SolvedPackage]
runSolver Service{..} env packageCache deps0 = resolve (toPackageMap deps0) (insertAll deps0 HashPSQ.empty)
  where
    insertAll deps queue =
      List.foldl'
        (\queue' pkgName -> HashPSQ.insert pkgName ((-1) * rankPackage pkgName) () queue')
        queue
        (Map.keys deps)

    resolve ::
      PackageMap -- ^ The dependency constraints so far
      -> PackageQueue
      -> ValidateT ConflictSet IO [SolvedPackage]
    resolve deps queue =
      case HashPSQ.minView queue of
        Nothing -> pure []
        Just (pkgName, _, _, queue') ->
          case lookupPackageMap pkgName deps of
            -- TODO: get `rts` version from ghc-pkg, should be treated the same as
            -- other packages not on Hackage (e.g. import package from GitHub)
            _ | pkgName == "rts" -> resolve deps queue'
            -- package is in the queue without registering a range; this is a bug
            Nothing -> error $ "Unexpectedly failed to find package " <> show pkgName <> ":\n" <> show deps
            Just (range, _)
              -- package already resolved
              | isSingletonRange range -> resolve deps queue'
              -- package not resolved
              | otherwise -> resolvePackage deps queue' pkgName range

    resolvePackage deps queue pkgName range = do
      PackageIndex.PackageVersionInfo{..} <- getPackageVersionInfoCached packageCache pkgName
      versions <-
        case filter (inRange range) availableVersions of
          [] -> do
            -- if there are no availableVersions in range, there must be a
            -- PackageOrigin with an unsatisfiable range; find it and error
            let hasInvalidRange PackageOrigin{range = originRange} =
                  case compileRange originRange of
                    Just range' -> all (not . inRange range') availableVersions
                    Nothing -> True
            case lookupPackageMap pkgName deps of
              Just (_, origins) | invalidOrigin : _ <- filter hasInvalidRange origins -> do
                let PackageOrigin{name = originName} = invalidOrigin
                ValidateT.refute $ Set.fromList [pkgName, originName]
              -- TODO: this can happen if someone puts "> 100" in hspackage.toml
              _ -> error $ "Unexpectedly failed to find any versions in range for " <> show pkgName
          versions -> do
            pure $ sortVersions env pkgName preferredVersionRange versions

      withBacktracking pkgName . flip map versions $ \pkgVer -> do
        let pkgId = PackageId pkgName pkgVer
        liftIO . logDebug loggingService $ "Trying package: " <> renderPackageId pkgId
        -- get package dependencies
        pkgDepsRaw <- getPackageDepsCached packageCache pkgId
        pkgDepsFull <-
          case traverse (\r -> (r,) <$> compileRange r) pkgDepsRaw of
            Just pkgDepsFull -> pure pkgDepsFull
            -- this package contains a dependency that has unsatisfiable bounds, so
            -- this package is completely unbuildable.
            -- e.g. aeson-1.5.5.0 => base < 0 && > 4.7
            Nothing -> ValidateT.refute $ Set.singleton pkgName
        -- update constraints
        deps' <-
          addErrors (Set.singleton pkgName) $
            mergeDeps
              loggingService
              pkgId
              pkgDepsFull
              (setVersionPackageMap pkgName pkgVer deps)
        -- recurse
        let pkgDeps = snd <$> pkgDepsFull
        let queue' = insertAll pkgDeps queue
        let pkg = SolvedPackage { packageId = pkgId, packageDeps = pkgDeps }
        (pkg :) <$> resolve deps' queue'

    withBacktracking :: PackageName -> [ValidateT ConflictSet IO a] -> ValidateT ConflictSet IO a
    withBacktracking pkgName = \case
      [] -> ValidateT.refute mempty
      m : ms -> m `catchErrors` \conflicts ->
        -- only continue if the current package-version is involved in the
        -- conflict; if it didn't, then that means some package lower in the
        -- search graph conflicted with a package higher in the search graph,
        -- so we bubble the error all the way up to a package involved in the
        -- conflict
        if pkgName `Set.member` conflicts
          then addErrors (Set.delete pkgName conflicts) $ withBacktracking pkgName ms
          else ValidateT.refute conflicts

-- | TODO: instead of re-sorting at the end, build an implication graph during solving
sortTopological :: PackageCache -> [SolvedPackage] -> IO [SolvedPackage]
sortTopological packageCache pkgs = do
  (graph, nodeFromVertex, _) <- Graph.graphFromEdges <$> mapM toNode pkgs
  pure . map (fromNode . nodeFromVertex) $ Graph.reverseTopSort graph
  where
    fromNode (node, _, _) = node
    toNode pkg@SolvedPackage{packageId} = do
      deps <- getPackageDepsCached packageCache packageId
      pure (pkg, packageName packageId, Map.keys deps)

-- | Order versions in the following order first, then latest to oldest.
--     1. Pre-installed versions
--     2. Preferred versions
--     3. Other versions
sortVersions ::
  CompilerEnv
  -> PackageName
  -> CompiledVersionRange
  -> [Version]
  -> [Version]
sortVersions env pkgName preferredVersionRange =
  List.sortOn $ \version -> (toVersionType version, Down version)
  where
    toVersionType :: Version -> Int
    toVersionType version
      | Just version == installedVersion = 1
      | inRange preferredVersionRange version = 2
      | otherwise = 100

    installedVersion = Map.lookup pkgName (CompilerEnv.ghcPkgList env)

-- | Rank the given package, where a higher rank means the package gets solved
-- earlier.
--
-- Heuristic:
--   1. Preinstalled packages
--   2. TODO
rankPackageDefault :: PackageName -> Int
rankPackageDefault name = fromMaybe (-1) $ name `elemIndex` reverse preinstalledPackages
  where
    -- Preinstalled packages, ordered from most important to least
    -- TODO: fill out more
    preinstalledPackages =
      [ "ghc"
      , "base"
      , "template-haskell"
      , "text"
      ]

{----- PackageMap -----}

-- | A map from package name to a version range and the origins of this dependency.
newtype PackageMap = PackageMap (Strict.Map PackageName (CompiledVersionRange, [PackageOrigin]))
  deriving (Show)

-- | For a dependency, the package/version that requested the dependency and the version range
-- that package wanted for this dependency.
data PackageOrigin = PackageOrigin
  { name :: PackageName
  , version :: Version
  , range :: VersionRange
  }
  deriving (Show)

toPackageMap :: PackageDeps -> PackageMap
toPackageMap = PackageMap . fmap (, []) . toStrictMap
  where
    toStrictMap = Map.Strict.fromAscList . Map.toAscList

lookupPackageMap :: PackageName -> PackageMap -> Maybe (CompiledVersionRange, [PackageOrigin])
lookupPackageMap name (PackageMap m) = Map.Strict.lookup name m

setVersionPackageMap :: PackageName -> Version -> PackageMap -> PackageMap
setVersionPackageMap name version (PackageMap m) =
  PackageMap (Map.Strict.adjust (first $ const (singletonRange version)) name m)

mergeDeps ::
  Logging.Service ->
  PackageId ->
  Map PackageName (VersionRange, CompiledVersionRange) ->
  PackageMap ->
  ValidateT ConflictSet IO PackageMap
mergeDeps loggingService pkgId newDeps (PackageMap deps) =
  PackageMap <$>
    Map.Strict.mergeA
      Map.Strict.preserveMissing
      Map.Strict.preserveMissing
      (Map.Strict.zipWithAMatched constrain)
      deps
      (Map.Strict.map addOrigin newDeps)
  where
    constrain depName (oldRange, origins1) (newRange, origins2) = do
      range <-
        case intersectRange oldRange newRange of
          Just r -> pure r
          Nothing -> do
            liftIO . logDebug loggingService . Text.intercalate "\n" $
              [ "Conflict: " <> renderPackageId pkgId <> " <=> " <> depName
              , "\tCurrent: " <> prettyCompiledRange oldRange
              , "\tNew:     " <> prettyCompiledRange newRange
              ]
            ValidateT.refute @_ @(ValidateT ConflictSet IO) $ Set.singleton depName

      pure (range, origins1 <> origins2)

    addOrigin (range, compiledRange) =
      let origin =
            PackageOrigin
              { name = packageName pkgId
              , version = packageVersion pkgId
              , range = range
              }
       in (compiledRange, [origin])

{----- Validation -----}

catchErrors :: Monad m => ValidateT e m a -> (e -> ValidateT e m a) -> ValidateT e m a
catchErrors m f =
  lift (runValidateT m) >>= \case
    Right a -> pure a
    Left e -> f e

addErrors :: (Monad m, Monoid e) => e -> ValidateT e m a -> ValidateT e m a
addErrors e1 = ValidateT.mapErrors (e1 <>)

{----- Cached package info -----}

data Cache k v = Cache
  { cacheMapVar :: MVar (LRU k v)
  , getVal :: k -> IO v
  }

initCache :: (MonadIO m, Ord k) => (k -> IO v) -> m (Cache k v)
initCache getVal = do
  cacheMapVar <- newMVar (LRU.newLRU $ Just 1000)
  pure Cache{..}

getCachedVal :: (MonadIO m, Ord k) => Cache k v -> k -> m v
getCachedVal Cache{..} k =
  liftIO . modifyMVar cacheMapVar $ \cacheMap -> do
    let (cacheMap', mVal) = LRU.lookup k cacheMap
    case mVal of
      Just v -> do
        pure (cacheMap', v)
      Nothing -> do
        v <- getVal k
        pure (LRU.insert k v cacheMap', v)

data PackageCache = PackageCache
  { packageVersionCache :: Cache PackageName PackageIndex.PackageVersionInfo
  , packageDependencyCache :: Cache PackageId RawPackageDeps
  }

initPackageCache ::
  MonadIO m =>
  (PackageName -> IO PackageIndex.PackageVersionInfo)
  -> (PackageId -> IO RawPackageDeps)
  -> m PackageCache
initPackageCache getVersionInfo getDeps = do
  packageVersionCache <- initCache getVersionInfo
  packageDependencyCache <- initCache getDeps
  pure PackageCache{..}

getPackageVersionInfoCached :: MonadIO m => PackageCache -> PackageName -> m PackageIndex.PackageVersionInfo
getPackageVersionInfoCached PackageCache{packageVersionCache} = getCachedVal packageVersionCache

getPackageDepsCached :: MonadIO m => PackageCache -> PackageId -> m RawPackageDeps
getPackageDepsCached PackageCache{packageDependencyCache} = getCachedVal packageDependencyCache
