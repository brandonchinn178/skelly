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

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
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
import Skelly.Core.Types.Version (compileRange, makeVersion)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Directory (doesFileExist, getCurrentDirectory)
import UnliftIO.Exception (fromEither)

data Service = Service
  { loggingService :: Logging.Service
  }

initService :: Logging.Service -> Service
initService loggingService =
  Service
    { loggingService
    }

{----- Options -----}

-- TODO: add command to update a single package version in the lock file (e.g. a transitive dep with a security vulnerability)
data Options = Options
  deriving (Show)

{----- Run -----}

run :: Service -> Options -> IO ()
run Service{..} _ = do
  config <- loadPackageConfig loggingService
  packages <- fromEither $ Map.fromList <$> mapM toPackageInfo [config]

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
      newLock <- updateLockFile env mLockFile packages
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
  CompilerEnv ->
  Maybe LockFile ->
  Map PackageName LockFilePackageInfo ->
  IO LockFile
updateLockFile env _ packages = do
  -- LockFileDepInfo
  --   { version :: Version
  --   , integrity :: Digest SHA256
  --   , deps :: Map Text VersionRange
  --   }
  let dependencies = mempty

  pure
    LockFile.LockFile
      { ghcVersion = CompilerEnv.ghcVersion env
      , packages
      , dependencies
      }
