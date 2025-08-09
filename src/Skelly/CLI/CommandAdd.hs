{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Skelly.CLI.CommandAdd (commandAdd) where

import Data.Text qualified as Text
import Options.Applicative qualified as Opt
import Skelly.CLI.Command
import Skelly.CLI.CommandBase
import Skelly.Core.Service (IsService (..), loadService)
import Skelly.Core.CompilerEnv (CompilerEnv, loadCompilerEnv)
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Logging (logDebug)
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.Solver qualified as Solver
import Skelly.Core.PackageConfig (PackageConfig)
import Skelly.Core.PackageConfig qualified as PackageConfig
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Types.PackageId (PackageName)
import Skelly.Core.Types.Version (
  Version,
  VersionOp (..),
  VersionRange (..),
  makeVersion,
  parseVersionRange,
  renderVersionRange,
 )
import UnliftIO.Exception (throwIO)

commandAdd :: CommandSpec '[BaseOptions]
commandAdd =
  CommandSpec
    { cmdName = "add"
    , cmdDesc = "Add dependencies to hspackage.toml"
    , cmdImpl = run
    , cmdOptions =
        pure Options
          <*> depsParser
    }
  where
    depsParser =
      Opt.many . Opt.argument parseDepArg . mconcat $
        [ Opt.metavar "DEP"
        , Opt.help "A dependency, optionally in the form of 'name@versionRange'"
        ]

    parseDepArg = do
      arg <- Opt.str
      case Text.splitOn "@" arg of
        [name] -> pure (name, Nothing)
        [name, rangeStr] ->
          case parseVersionRange rangeStr of
            Just range -> pure (name, Just range)
            Nothing -> Opt.readerError $ Text.unpack $ "Range specified with invalid format: " <> rangeStr
        _ -> Opt.readerError $ Text.unpack $ "Dependency specified with invalid format: " <> arg

instance
  ( IsService opts Logging.Service
  , IsService opts PackageIndex.Service
  ) => IsService opts Service where
  initService = do
    loggingService <- loadService
    packageIndexService <- loadService
    pure
      Service
        { loggingService
        , loadPackageConfig = PackageConfig.load loggingService
        , savePackageConfig = PackageConfig.save
        , getPreferredVersion = \env pkgName ->
            PackageIndex.withCursor packageIndexService $ \cursor -> do
              PackageIndex.PackageVersionInfo{..} <- PackageIndex.getPackageVersionInfo cursor pkgName
              case Solver.sortVersions env pkgName preferredVersionRange availableVersions of
                v : _ -> pure v
                [] -> throwIO $ NoValidVersions pkgName availableVersions preferredVersionRange
        }

{----- Execution -----}

data Service = Service
  { loggingService :: Logging.Service
  , loadPackageConfig :: IO PackageConfig
  , savePackageConfig :: PackageConfig -> IO ()
  , getPreferredVersion :: CompilerEnv -> PackageName -> IO Version
  }

-- TODO: specify which lib/bin/test to add dep to
data Options = Options
  { dependencies :: [(PackageName, Maybe VersionRange)]
  }
  deriving (Show)

run :: Service -> Options -> IO ()
run Service{..} Options{..} = do
  cfg <- loadPackageConfig
  let ghcVersion = makeVersion [9, 10, 1] -- TODO: get from hspackage.toml
  env <- loadCompilerEnv ghcVersion
  foldlM (go env) cfg dependencies >>= savePackageConfig
  where
    go env cfg (dep, mRange) = do
      range <- resolveRange env dep mRange
      logDebug loggingService $
        "Adding dependency: " <> dep <> " => " <> renderVersionRange range
      pure $ PackageConfig.addDependency dep range cfg

    -- If a range isn't specified, default to "^X.Y.Z", where "X.Y.Z" is the
    -- most recent version on Hackage currently.
    resolveRange env dep = \case
      Just range -> pure range
      -- TODO: ensure version is compatible with other bounds
      -- TODO: if package already in deps, update the version
      Nothing -> VersionWithOp VERSION_PVP_MAJOR <$> getPreferredVersion env dep

    foldlM f z = \case
      [] -> pure z
      x : xs -> do
        result <- f z x
        foldlM f result xs
