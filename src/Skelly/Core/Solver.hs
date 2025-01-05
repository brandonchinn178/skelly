{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
  PackageDeps,
  run,

  -- * Helpers
  rankPackageDefault,
) where

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class (MonadIO (..))
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Skelly.Core.Error (
  SkellyError (
    DependencyResolutionFailure,
    UnsatisfiableVersionRange
  ),
 )
import Skelly.Core.Logging (logDebug)
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Utils.PackageId (
  PackageId (..),
  PackageName,
  renderPackageId,
 )
import Skelly.Core.Utils.Version (
  CompiledVersionRange,
  Version,
  VersionRange,
  getVersionPreferences,
  compileRange,
  intersectRange,
  isSingletonRange,
  renderCompiledRange,
  singletonRange,
 )
import UnliftIO.MVar (MVar, newMVar, modifyMVar)
import UnliftIO.Exception (throwIO)

type PackageDeps = Map PackageName VersionRange

data Service = Service
  { loggingService :: Logging.Service
  , withCursor :: forall a. (PackageIndex.PackageIndexCursor -> IO a) -> IO a
  , getPackageDeps :: PackageIndex.PackageIndexCursor -> PackageId -> IO PackageDeps
  , getPackageVersions :: PackageIndex.PackageIndexCursor -> PackageName -> IO [Version]
  , -- | Rank package, where higher is better.
    rankPackage :: PackageName -> Int
  }

initService :: Logging.Service -> PackageIndex.Service -> Service
initService loggingService pkgIndexService =
  Service
    { loggingService
    , withCursor = PackageIndex.withCursor pkgIndexService
    , getPackageDeps = \cursor -> fmap PackageIndex.packageDependencies . PackageIndex.getPackageInfo cursor
    , getPackageVersions = \cursor -> fmap PackageIndex.availableVersions . PackageIndex.getPackageVersionInfo cursor
    , rankPackage = rankPackageDefault
    }

-- | Get the list of transitive dependencies in topological order, where
-- packages may not depend on any packages later in the list.
run :: Service -> PackageDeps -> IO [PackageId]
run service@Service{..} initialDeps =
  withCursor $ \cursor -> do
    packageCache <- initPackageCache (getPackageVersions cursor) (getPackageDeps cursor)
    initialDeps' <- compileDeps initialDeps
    runValidateT (runSolver service packageCache initialDeps') >>= \case
      Right packages -> sortTopological packageCache packages
      Left _ -> throwIO DependencyResolutionFailure
  where
    compileDeps = Map.traverseWithKey $ \package range ->
      case compileRange range of
        Just range' -> pure range'
        Nothing -> throwIO $ UnsatisfiableVersionRange package range

type ConflictSet = Set PackageName
type PackageQueue = HashPSQ PackageName Int ()

runSolver ::
  Service
  -> PackageCache
  -> Map PackageName CompiledVersionRange
  -> ValidateT IO ConflictSet [PackageId]
runSolver Service{..} packageCache deps0 = resolve (toStrictMap deps0) (insertAll deps0 HashPSQ.empty)
  where
    toStrictMap = Map.Strict.fromAscList . Map.toAscList
    insertAll deps queue =
      List.foldl'
        (\queue' pkgName -> HashPSQ.insert pkgName ((-1) * rankPackage pkgName) () queue')
        queue
        (Map.keys deps)

    resolve ::
      Strict.Map PackageName CompiledVersionRange -- ^ The dependency constraints so far
      -> PackageQueue
      -> ValidateT IO ConflictSet [PackageId]
    resolve deps queue =
      case HashPSQ.minView queue of
        Nothing -> pure []
        Just (pkgName, _, _, queue') ->
          case Map.Strict.lookup pkgName deps of
            -- rts package doesn't actually exist
            _ | pkgName == "rts" -> resolve deps queue'
            -- package is in the queue without registering a range; this is a bug
            Nothing -> error $ "Unexpectedly failed to find package " <> show pkgName <> ":\n" <> show deps
            Just range
              -- package already resolved
              | isSingletonRange range -> resolve deps queue'
              -- package not resolved
              | otherwise -> resolvePackage deps queue' pkgName range

    resolvePackage deps queue pkgName range = do
      versions <- getVersionPreferences range <$> getPackageVersionsCached packageCache pkgName
      withBacktracking pkgName . flip map versions $ \pkgVer -> do
        let pkgId = PackageId pkgName pkgVer
        liftIO . logDebug loggingService $ "Trying package: " <> renderPackageId pkgId
        -- get package dependencies
        pkgDepsRaw <- getPackageDepsCached packageCache pkgId
        pkgDeps <-
          case traverse compileRange pkgDepsRaw of
            Just pkgDeps -> pure pkgDeps
            -- this package contains a dependency that has unsatisfiable bounds, so
            -- this package is completely unbuildable.
            -- e.g. aeson-1.5.5.0 => base < 0 && > 4.7
            Nothing -> refute $ Set.singleton pkgName
        -- update constraints
        deps' <-
          mapErrors (Set.insert pkgName) $
            mergeDeps
              pkgId
              (Map.Strict.insert pkgName (singletonRange pkgVer) deps)
              pkgDeps
        -- recurse
        let queue' = insertAll pkgDeps queue
        (pkgId :) <$> resolve deps' queue'

    withBacktracking :: PackageName -> [ValidateT IO ConflictSet a] -> ValidateT IO ConflictSet a
    withBacktracking pkgName = \case
      [] -> empty
      m : ms -> m `catchErrors` \conflicts ->
        -- only continue if the current package-version is involved in the
        -- conflict; if it didn't, then that means some package lower in the
        -- search graph conflicted with a package higher in the search graph,
        -- so we bubble the error all the way up to a package involved in the
        -- conflict
        if pkgName `Set.member` conflicts
          then addErrors (Set.delete pkgName conflicts) $ withBacktracking pkgName ms
          else refute conflicts

    mergeDeps pkgId =
      let constrain depName r1 r2 =
            case intersectRange r1 r2 of
              Just r -> pure r
              Nothing -> do
                let oldRange = renderCompiledRange r1
                    newRange = renderCompiledRange r2
                liftIO . logDebug loggingService . Text.intercalate "\n" $
                  [ "Conflict: " <> renderPackageId pkgId <> " <=> " <> depName
                  , "\tCurrent: " <> oldRange
                  , "\tNew:     " <> newRange
                  ]
                refute $ Set.singleton depName
       in Map.Strict.mergeA
            Map.Strict.preserveMissing
            Map.Strict.preserveMissing
            (Map.Strict.zipWithAMatched constrain)

-- | TODO: instead of re-sorting at the end, build an implication graph during solving
sortTopological :: PackageCache -> [PackageId] -> IO [PackageId]
sortTopological packageCache pkgs = do
  (graph, nodeFromVertex, _) <- Graph.graphFromEdges <$> mapM toNode pkgs
  pure . map (fromNode . nodeFromVertex) $ Graph.reverseTopSort graph
  where
    fromNode (node, _, _) = node
    toNode pkgId@PackageId{packageName} = do
      deps <- getPackageDepsCached packageCache pkgId
      pure (pkgId, packageName, Map.keys deps)

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

{----- Validation -----}

newtype ValidateT m e a = ValidateT (m (Either e a))

instance Functor m => Functor (ValidateT m e) where
  fmap f (ValidateT m) = ValidateT $ (fmap . fmap) f m
instance (Applicative m, Monoid e) => Applicative (ValidateT m e) where
  pure = ValidateT . pure . Right
  ValidateT mf <*> ValidateT mx = ValidateT $
    ( \ef ex ->
        case (ef, ex) of
          (Left e1, Left e2) -> Left (e1 <> e2)
          (Left e, Right _) -> Left e
          (Right _, Left e) -> Left e
          (Right f, Right x) -> Right (f x)
    )
      <$> mf
      <*> mx
instance (Monad m, Monoid e) => Monad (ValidateT m e) where
  ValidateT ma >>= k = ValidateT $
    ma >>= \case
      Left e -> pure $ Left e
      Right a -> runValidateT $ k a
instance (Monad m, Monoid e) => Alternative (ValidateT m e) where
  empty = ValidateT (pure $ Left mempty)
  m1 <|> m2 = m1 `catchErrors` \e -> addErrors e m2
instance (MonadIO m, Monoid e) => MonadIO (ValidateT m e) where
  liftIO = ValidateT . liftIO . fmap Right

runValidateT :: ValidateT m e a -> m (Either e a)
runValidateT (ValidateT m) = m

refute :: Applicative m => e -> ValidateT m e a
refute = ValidateT . pure . Left

catchErrors :: Monad m => ValidateT m e a -> (e -> ValidateT m e a) -> ValidateT m e a
catchErrors (ValidateT m) f = ValidateT $
  m >>= \case
    Right x -> pure $ Right x
    Left e -> runValidateT $ f e

addErrors :: (Functor m, Monoid e) => e -> ValidateT m e a -> ValidateT m e a
addErrors e1 = mapErrors (e1 <>)

mapErrors :: Functor m => (e1 -> e2) -> ValidateT m e1 a -> ValidateT m e2 a
mapErrors f (ValidateT m) = ValidateT $ first f <$> m

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
  { packageVersionCache :: Cache PackageName [Version]
  , packageDependencyCache :: Cache PackageId PackageDeps
  }

initPackageCache ::
  MonadIO m =>
  (PackageName -> IO [Version])
  -> (PackageId -> IO PackageDeps)
  -> m PackageCache
initPackageCache getVersions getDeps = do
  packageVersionCache <- initCache getVersions
  packageDependencyCache <- initCache getDeps
  pure PackageCache{..}

getPackageVersionsCached :: MonadIO m => PackageCache -> PackageName -> m [Version]
getPackageVersionsCached PackageCache{packageVersionCache} = getCachedVal packageVersionCache

getPackageDepsCached :: MonadIO m => PackageCache -> PackageId -> m PackageDeps
getPackageDepsCached PackageCache{packageDependencyCache} = getCachedVal packageDependencyCache
