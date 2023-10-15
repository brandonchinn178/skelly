{-# LANGUAGE LambdaCase #-}

module Skelly.Core.Utils.Cabal (
  parsePreferredVersions,

  -- * Package names
  Cabal.PackageIdentifier (..),
  Cabal.mkPackageName,
  fromPackageName,
  toPackageName,

  -- * Versions
  fromCabalVersion,
) where

import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Distribution.Client.IndexUtils qualified as Cabal
import Distribution.Types.Dependency qualified as Cabal
import Distribution.Types.PackageId qualified as Cabal
import Distribution.Types.PackageName qualified as Cabal
import Distribution.Types.Version qualified as Cabal
import Distribution.Types.VersionRange qualified as Cabal
import Skelly.Core.Utils.Version (
  Version,
  VersionOp (..),
  VersionRange (..),
  makeVersion,
 )

-- | Get the preferred versions for the given package
-- in the given 'preferred-versions' file.
parsePreferredVersions :: Text -> Lazy.ByteString -> Maybe VersionRange
parsePreferredVersions package =
  listToMaybe
    . mapMaybe go
    . Cabal.parsePreferredVersionsWarnings
  where
    go = \case
      Right (Cabal.Dependency pkg range _)
        | Cabal.unPackageName pkg == Text.unpack package ->
            Just $ fromRange range
      _ -> Nothing

    fromRange =
      Cabal.foldVersionRange
        AnyVersion
        (VersionWithOp VERSION_EQ . fromCabalVersion)
        (VersionWithOp VERSION_GT . fromCabalVersion)
        (VersionWithOp VERSION_LT . fromCabalVersion)
        VersionRangeOr
        VersionRangeAnd

fromCabalVersion :: Cabal.Version -> Version
fromCabalVersion = makeVersion . Cabal.versionNumbers

fromPackageName :: Cabal.PackageName -> Text
fromPackageName = Text.pack . Cabal.unPackageName

toPackageName :: Text -> Cabal.PackageName
toPackageName = Cabal.mkPackageName . Text.unpack
