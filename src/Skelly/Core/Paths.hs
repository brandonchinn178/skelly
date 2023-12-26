module Skelly.Core.Paths (
  skellyConfigDir,
  skellyCacheDir,
  packageDistDir,
) where

import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Directory (XdgDirectory (..), getXdgDirectory)

{-# NOINLINE skellyConfigDir #-}
skellyConfigDir :: FilePath
skellyConfigDir = unsafePerformIO $ getXdgDirectory XdgConfig "skelly"

{-# NOINLINE skellyCacheDir #-}
skellyCacheDir :: FilePath
skellyCacheDir = unsafePerformIO $ getXdgDirectory XdgCache "skelly"

packageDistDir :: FilePath
packageDistDir = "dist"
