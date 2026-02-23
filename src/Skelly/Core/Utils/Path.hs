{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Utils.Path (
  listFiles,
  ListFilesOptions (..),
  module X,
) where

import Skelly.Core.Utils.Default (DefaultOptions (..))
import System.Directory as X
import System.FilePath ((</>))

data ListFilesOptions = ListFilesOptions
  { listFilesFollowSymlinks :: Bool
  , listFilesExcludeDirs :: [FilePath]
  }

instance DefaultOptions ListFilesOptions where
  defaultOpts =
    ListFilesOptions
      { listFilesFollowSymlinks = False
      , listFilesExcludeDirs = []
      }

listFiles :: ListFilesOptions -> FilePath -> IO [FilePath]
listFiles ListFilesOptions{..} = go
 where
  go dir = do
    entries <- listDirectory dir
    flip concatMapM entries $ \entryName -> do
      let entryPath = dir </> entryName
      shouldRecurse <-
        doesDirectoryExist entryPath
          <&&> (pure listFilesFollowSymlinks <||> (not <$> pathIsSymbolicLink entryPath))
          <&&> pure (entryName `notElem` listFilesExcludeDirs)
      if shouldRecurse
        then map (entryName </>) <$> go entryPath
        else pure [entryName]

  concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
  concatMapM f = fmap concat . mapM f

  (<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
  m1 <&&> m2 = do
    b <- m1
    if b then m2 else pure False

  (<||>) :: (Monad m) => m Bool -> m Bool -> m Bool
  m1 <||> m2 = do
    b <- m1
    if b then pure True else m2
