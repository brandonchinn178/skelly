{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Utils.InstalledPackageInfo (
  InstalledPackageInfo (..),
  renderInstalledPackageInfo,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Skelly.Core.Types.PackageId (PackageId (..), renderPackageId)
import Skelly.Core.Types.Version (renderVersion)
import Skelly.Core.Utils.Modules (ModuleName, renderModuleName)

data InstalledPackageInfo = InstalledPackageInfo
  { installedPackageId :: PackageId
  , installedPackageLocation :: FilePath
  , installedPackageModules :: [ModuleName]
  , installedPackageDeps :: [PackageId]
  }

-- | Generate the contents for the file containing information for an installed
-- package.
--
-- We could use showInstalledPackageInfo from Cabal-syntax, but reimplementing
-- here to reduce dependence on the Cabal library.
--
-- https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/packages.html#installedpackageinfo-a-package-specification
renderInstalledPackageInfo :: InstalledPackageInfo -> Text
renderInstalledPackageInfo InstalledPackageInfo{..} =
    Text.unlines
      [ "name: " <> packageName
      , "version: " <> renderVersion packageVersion
      , "id: " <> packageId
      , "key: " <> packageId -- https://github.com/ghc/ghc/commit/d80caca10d7c2fa1c9ee8ef6bcafac365d02ff3d
      -- TODO: license
      -- TODO: copyright
      -- TODO: author
      -- TODO: maintainer
      -- TODO: stability
      -- TODO: homepage
      -- TODO: package-url
      -- TODO: description
      -- TODO: category
      , "exposed: True"
      , "exposed-modules:" <> stringList (map renderModuleName installedPackageModules)
      -- TODO: hidden modules
      -- TODO: trusted
      , "import-dirs: " <> Text.pack installedPackageLocation
      , "library-dirs: " <> Text.pack installedPackageLocation
      , "hs-libraries: " <> ("HS" <> packageId)
      -- TODO: extra-libraries
      -- TODO: extra-ghci-libraries
      -- TODO: include-dirs
      -- TODO: includes
      , "depends: " <> stringList (map renderPackageId installedPackageDeps)
      -- TODO: hugs-options
      -- TODO: cc-options
      -- TODO: ld-options
      -- TODO: framework-dirs
      -- TODO: frameworks
      -- TODO: haddock-interfaces
      -- TODO: haddock-html
      ]
  where
    PackageId{packageName, packageVersion} = installedPackageId
    packageId = renderPackageId installedPackageId

    stringList = Text.concat . map ("\n  " <>) . sep ","
    sep s = \case
      [] -> []
      [x] -> [x]
      x:xs -> (x <> s) : sep s xs
