{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skelly.Core.Error (
  SkellyError (..),
  SomeHackageError (..),
  renderSkellyError,
) where

import Control.Exception (Exception (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Skelly.Core.Utils.PackageId (
  PackageId,
  renderPackageId,
 )
import Skelly.Core.Utils.Version (
  Version,
  VersionRange,
  renderVersion,
  renderVersionRange,
 )

data SkellyError
  = NoPackageConfig [FilePath]
  | SomeHackageError SomeHackageError
  | UnknownPackage Text
  | PackageIdNotFound PackageId
  | NoValidVersions Text [Version] VersionRange
  | BadCabalFile PackageId Text
  deriving (Show)

instance Exception SkellyError where
  displayException = Text.unpack . renderSkellyError

data SomeHackageError = forall e. Exception e => MkSomeHackageError e

instance Show SomeHackageError where
  show (MkSomeHackageError e) = show e

renderSkellyError :: SkellyError -> Text
renderSkellyError = \case
  NoPackageConfig locs ->
    Text.unlines $
      "Could not find hspackage.toml in any of the following locations:"
        : ["  - " <> Text.pack loc | loc <- locs]
  SomeHackageError (MkSomeHackageError e) ->
    "Got a Hackage error: " <> Text.pack (displayException e)
  UnknownPackage package ->
    "Unknown package: " <> package
  PackageIdNotFound packageId ->
    "Package not found: " <> renderPackageId packageId
  NoValidVersions package availableVersions versionRange ->
    Text.unlines
      [ "Package " <> package <> " has no versions in the range " <> renderVersionRange versionRange <> ":"
      , "Available versions: " <> Text.intercalate ", " (map renderVersion availableVersions)
      ]
  BadCabalFile packageId msg ->
    Text.unlines
      [ "Could not parse cabal file for " <> renderPackageId packageId <> ":"
      , msg
      ]
