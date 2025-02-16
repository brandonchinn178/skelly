{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.CompilerEnv (
  CompilerEnv (..),
  loadCompilerEnv,
) where

import Control.Monad (unless)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Types.PackageId (
  PackageId (..),
  PackageName,
  parsePackageId,
 )
import Skelly.Core.Types.Version (Version, parseVersion, renderVersion)
import System.Directory (canonicalizePath, doesFileExist, findExecutables)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess)
import UnliftIO.Exception (throwIO, tryAny)

data CompilerEnv = CompilerEnv
  { ghcPath :: FilePath
  , ghcVersion :: Version
  , ghcPkgPath :: FilePath
  , ghcPkgList :: Map PackageName Version
  }

-- | Load the environment for the given version of GHC.
loadCompilerEnv :: Version -> IO CompilerEnv
loadCompilerEnv ghcVersion = do
  ghcPath <- findGhc
  ghcPkgPath <- findGhcPkg ghcPath
  ghcPkgList <- loadGhcPkgList ghcPkgPath
  pure
    CompilerEnv
      { ghcPath
      , ghcVersion
      , ghcPkgPath
      , ghcPkgList
      }
  where
    findGhc = do
      exes <-
        fmap concat . mapM findExecutables $
          [ "ghc-" <> (Text.unpack . renderVersion) ghcVersion
          , "ghc"
          ]

      let isMatch exe =
            readLines exe ["--version"] >>= \case
              [line] -> do
                let versionStr = Text.takeWhileEnd (/= ' ') line
                pure $ parseVersion versionStr == Just ghcVersion
              _ -> do
                pure False

      findM isMatch exes >>= \case
        Just exe -> canonicalizePath exe
        Nothing -> throwIO $ ExecutableNotFound ("ghc-" <> renderVersion ghcVersion)

    findGhcPkg ghc = do
      let ghcPkg = takeDirectory ghc </> "ghc-pkg"
      ghcPkgExists <- doesFileExist ghcPkg
      unless ghcPkgExists $ throwIO $ ExecutableNotFound (Text.pack ghcPkg)
      pure ghcPkg

    loadGhcPkgList ghcPkg = do
      pkgs <- map Text.strip . drop 1 <$> readLines ghcPkg ["list"]
      pure . Map.fromList $
        [ (packageName, packageVersion)
        | Just PackageId{..} <- map parsePackageId pkgs
        ]

    readLines cmd args =
      tryAny (readProcess cmd args "") >>= \case
        Left _ -> pure []
        Right s -> pure . Text.lines . Text.pack $ s

    findM f = \case
      [] -> pure Nothing
      x : xs -> do
        cond <- f x
        if cond
          then pure $ Just x
          else findM f xs
