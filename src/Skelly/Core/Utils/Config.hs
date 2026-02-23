{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Skelly.Core.Utils.Config (
  findConfig,
) where

import Data.Text qualified as Text
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Logging qualified as Logging
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Directory (doesFileExist, getCurrentDirectory)
import UnliftIO.Exception (throwIO)

findConfig :: Logging.Service -> FilePath -> IO FilePath
findConfig loggingService configName = do
  paths <- getConfigPaths <$> getCurrentDirectory
  findM doesFileExist paths >>= \case
    Nothing -> throwIO $ NoConfig configName paths
    Just fp -> do
      loggingService.debug . Text.pack $ "Found " <> configName <> " at: " <> fp
      pure fp
 where
  getConfigPaths dir =
    let parent = takeDirectory dir
        parents = if parent == dir then [] else getConfigPaths parent
     in (dir </> configName) : parents

  findM f = \case
    [] -> pure Nothing
    x : xs -> do
      res <- f x
      if res then pure (Just x) else findM f xs
