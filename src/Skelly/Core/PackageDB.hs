{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Skelly.Core.PackageDB (
  PackageDB,
  packageDbPath,
  loadPackageDB,
  packageDbEntries,
  registerPackageInDB,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Skelly.Core.Utils.InstalledPackageInfo (
  InstalledPackageInfo (..),
  renderInstalledPackageInfo,
 )
import Skelly.Core.Utils.PackageId (PackageId (..), renderPackageId)
import Skelly.Core.Utils.Version (parseVersion)
import System.Directory (createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))
import System.Process (callProcess, readProcess, readProcessWithExitCode)

-- | The package database is a registry of all Haskell packages installed
-- for a given project. Each project has its own package database.
newtype PackageDB = PackageDB FilePath

packageDbPath :: PackageDB -> FilePath
packageDbPath (PackageDB path) = path

-- | Load the package database in the given output directory.
loadPackageDB :: FilePath -> IO PackageDB
loadPackageDB distDir = do
  let path = distDir </> "package.db"

  -- for now, symlink the global DB and assume deps are already built
  -- TODO: delete this
  doesDirectoryExist path >>= \case
    True -> pure ()
    False -> do
      createDirectoryIfMissing True distDir
      homeDir <- getHomeDirectory
      createDirectoryLink (homeDir </> ".cabal/store/ghc-9.6.3/package.db") path

  -- initialize the path if it doesn't already exist, ignore the error if path already exists
  _ <- readProcessWithExitCode "ghc-pkg" ["init", path] ""

  pure $ PackageDB path

packageDbEntries :: PackageDB -> IO (Map Text PackageId)
packageDbEntries (PackageDB path) = do
  output <- readProcess "ghc-pkg" ["list", "-f", path, "--no-user-package-db", "--simple-output"] ""
  packages <-
    case Text.lines (Text.pack output) of
      [line] -> pure $ Text.words line
      _ -> error $ "ghc-pkg returned unexpected output:\n" <> output
  pure . Map.fromList $
    flip map packages $ \s ->
      case Text.breakOnEnd "-" s of
        (prefix, versionStr) | Just version <- parseVersion versionStr ->
          let pkg = Text.dropEnd 1 prefix
              pkgId =
                PackageId
                  { packageName = pkg
                  , packageVersion = version
                  }
           in (pkg, pkgId)
        _ -> error $ "ghc-pkg returned an unknown package: " <> show s

registerPackageInDB :: PackageDB -> InstalledPackageInfo -> IO ()
registerPackageInDB (PackageDB path) info = do
  Text.writeFile (path </> packageConfFile) (renderInstalledPackageInfo info)
  callProcess "ghc-pkg" ["recache", "-f", path]
  where
    packageId = installedPackageId info
    packageConfFile = Text.unpack (renderPackageId packageId) <> ".conf"
