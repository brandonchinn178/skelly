module Skelly.Core.Paths (
  skellyConfigDir,
  skellyCacheDir,
  packageDistDir,
) where

import Data.Text qualified as Text
import Skelly.Core.Utils.Version (Version, renderVersion)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Directory (XdgDirectory (..), getXdgDirectory)

{-# NOINLINE skellyConfigDir #-}
skellyConfigDir :: FilePath
skellyConfigDir = unsafePerformIO $ getXdgDirectory XdgConfig "skelly"

{-# NOINLINE skellyCacheDir #-}
skellyCacheDir :: FilePath
skellyCacheDir = unsafePerformIO $ getXdgDirectory XdgCache "skelly"

packageDistDir :: FilePath -> Version -> FilePath
packageDistDir projectDir ghcVersion =
  projectDir
    </> "dist"
    </> ("ghc-" <> Text.unpack (renderVersion ghcVersion))
