{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.CommandAdd (commandAdd) where

import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
import Skelly.CLI.Command
import Skelly.CLI.Service qualified as CLI
import Skelly.Core.Logging (logDebug)
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageConfig (PackageConfig)
import Skelly.Core.PackageConfig qualified as PackageConfig
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Utils.Version (
  Version,
  VersionOp (..),
  VersionRange (..),
  parseVersionRange,
  renderVersionRange,
 )

commandAdd :: Command
commandAdd =
  Command
    { cmdName = "add"
    , cmdDesc = "Add dependencies to hspackage.toml"
    , cmdParse =
        pure Options
          <*> depsParser
    , cmdExec = execute
    }
  where
    depsParser =
      many . argument parseDepArg . mconcat $
        [ metavar "DEP"
        , help "A dependency, optionally in the form of 'name@versionRange'"
        ]

    parseDepArg = do
      arg <- str
      case Text.splitOn "@" arg of
        [name] -> pure (name, Nothing)
        [name, rangeStr] ->
          case parseVersionRange rangeStr of
            Just range -> pure (name, Just range)
            Nothing -> readerError $ Text.unpack $ "Range specified with invalid format: " <> rangeStr
        _ -> readerError $ Text.unpack $ "Dependency specified with invalid format: " <> arg

execute :: CLI.Service -> Options -> IO ()
execute CLI.Service{..} = run service
  where
    service =
      Service
        { loggingService
        , loadPackageConfig = PackageConfig.loadPackageConfig loggingService
        , savePackageConfig = PackageConfig.savePackageConfig
        , getLatestVersion = PackageIndex.getLatestVersion packageIndexService
        }

{----- Execution -----}

data Service = Service
  { loggingService :: Logging.Service
  , loadPackageConfig :: IO PackageConfig
  , savePackageConfig :: PackageConfig -> IO ()
  , getLatestVersion :: Text -> IO Version
  }

-- TODO: specify which lib/bin/test to add dep to
data Options = Options
  { dependencies :: [(Text, Maybe VersionRange)]
  }
  deriving (Show)

run :: Service -> Options -> IO ()
run Service{..} Options{..} = do
  cfg <- loadPackageConfig
  foldlM go cfg dependencies >>= savePackageConfig
  where
    go cfg (dep, mRange) = do
      range <- resolveRange dep mRange
      logDebug loggingService $
        "Adding dependency: " <> dep <> " => " <> renderVersionRange range
      pure $ PackageConfig.addDependency dep range cfg

    -- If a range isn't specified, default to "^X.Y.Z", where "X.Y.Z" is the
    -- most recent version on Hackage currently.
    resolveRange dep = \case
      Just range -> pure range
      -- TODO: ensure version is compatible with other bounds
      -- TODO: if package already in deps, update the version
      Nothing -> VersionWithOp VERSION_PVP_MAJOR <$> getLatestVersion dep

    foldlM f z = \case
      [] -> pure z
      x : xs -> do
        result <- f z x
        foldlM f result xs
