{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Lock (
  -- * Service
  Service (..),
  initService,

  -- * Methods
  run,
  Options (..),
) where

import Control.Monad (join)
import Crypto.Hash qualified as Crypto
import Data.ByteString qualified as ByteString
import Data.Map (Map)
import Data.Map qualified as Map
import Skelly.Core.CompilerEnv (CompilerEnv, loadCompilerEnv)
import Skelly.Core.CompilerEnv qualified as CompilerEnv
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Lock.LockFile (
  LockFile,
  LockFilePackageInfo,
  readLockFile,
  writeLockFile,
 )
import Skelly.Core.Lock.LockFile qualified as LockFile
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageConfig (PackageConfig, loadPackageConfig)
import Skelly.Core.PackageConfig qualified as PackageConfig
import Skelly.Core.Paths (skellyLockFile)
import Skelly.Core.Solver qualified as Solver
import Skelly.Core.Types.PackageId (PackageId (..), PackageName)
import Skelly.Core.Types.Version (CompiledVersionRange, compileRange, intersectRange, makeVersion)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Directory (doesFileExist, getCurrentDirectory)
import UnliftIO.Exception (fromEither, throwIO)

data Service = Service
  { loggingService :: Logging.Service
  , solveDeps :: CompilerEnv -> Map PackageName CompiledVersionRange -> IO [Solver.SolvedPackage]
  }

initService :: Logging.Service -> Solver.Service -> Service
initService loggingService solverService =
  Service
    { loggingService
    , solveDeps = Solver.run solverService
    }

{----- Options -----}

-- TODO: add command to update a single package version in the lock file (e.g. a transitive dep with a security vulnerability)
data Options = Options
  deriving (Show)

{----- Run -----}

run :: Service -> Options -> IO ()
run service@Service{..} _ = do
  config <- loadPackageConfig loggingService
  packages <- fromEither $ Map.fromList <$> mapM toPackageInfo [config] -- TODO: load all configs in workspace

  let ghcVersion = makeVersion [9, 10, 1] -- TODO: decide version from hspackage.toml
  env <- loadCompilerEnv ghcVersion

  lockExists <- doesFileExist lockFilePath
  status <-
    if not lockExists
      then pure $ LockFileOutdated Nothing
      else do
        lockFile <- readLockFile lockFilePath
        if LockFile.packages lockFile /= packages
          then pure $ LockFileOutdated (Just lockFile)
          else pure LockFileUpToDate

  case status of
    LockFileOutdated mLockFile -> do
      newLock <- updateLockFile service env mLockFile packages
      writeLockFile lockFilePath newLock
      putStrLn "Lock file updated."
    LockFileUpToDate -> do
      putStrLn "Lock file up-to-date."
  where
    -- TODO: get actual directory where the hsproject.toml file is
    projectDir = unsafePerformIO getCurrentDirectory
    lockFilePath = projectDir </> skellyLockFile

data LockFileStatus
  = LockFileOutdated (Maybe LockFile)
  | LockFileUpToDate

toPackageInfo :: PackageConfig -> Either SkellyError (PackageName, LockFilePackageInfo)
toPackageInfo config = do
  let name = PackageConfig.packageName config
  deps <-
    flip Map.traverseWithKey (PackageConfig.allDependencies config) $ \package range ->
      case compileRange range of
        Just r -> Right r
        Nothing -> Left $ UnsatisfiableVersionRange package range
  pure (name, LockFile.LockFilePackageInfo{..})

updateLockFile ::
  Service ->
  CompilerEnv ->
  Maybe LockFile ->
  Map PackageName LockFilePackageInfo ->
  IO LockFile
updateLockFile Service{..} env _ packages = do
  deps <- solveDeps env =<< mergeDeps (Map.elems packages)
  let dependencies =
        [ (packageName packageId, info)
        | Solver.SolvedPackage{..} <- deps
        , let info =
                LockFile.LockFileDepInfo
                  { version = packageVersion packageId
                  , integrity = Crypto.hash ByteString.empty -- TODO: get checksum of all files
                  , deps = packageDeps
                  }
        ]

  pure
    LockFile.LockFile
      { ghcVersion = CompilerEnv.ghcVersion env
      , packages
      , dependencies = Map.fromList dependencies
      }
  where
    mergeDeps =
      maybe (throwIO DependencyResolutionFailure) pure
        . unionsWithM intersectRange
        . map (\LockFile.LockFilePackageInfo{deps} -> deps)

    unionsWithM :: (Monad m, Ord k) => (a -> a -> m a) -> [Map k a] -> m (Map k a)
    unionsWithM f =
      let f' m1 m2 = join $ f <$> m1 <*> m2
       in sequence . Map.unionsWith f' . map (fmap pure)
