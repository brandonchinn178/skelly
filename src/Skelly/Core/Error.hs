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
  PackageName,
  renderPackageId,
 )
import Skelly.Core.Utils.Version (
  CompiledVersionRange,
  Version,
  VersionRange,
  renderVersion,
  renderCompiledRange,
  renderVersionRange,
 )

data SkellyError
  = NoPackageConfig [FilePath]
  | ExecutableNotFound Text
  | SomeHackageError SomeHackageError
  | UnknownPackage PackageName
  | PackageIdNotFound PackageId
  | NoValidVersions PackageName [Version] CompiledVersionRange
  | BadCabalFile PackageId Text
  | DependencyResolutionFailure -- ^ TODO: add conflict info
  | UnsatisfiableVersionRange PackageName VersionRange
  deriving (Show, Eq)

instance Exception SkellyError where
  displayException = Text.unpack . renderSkellyError

data SomeHackageError = forall e. Exception e => MkSomeHackageError e

instance Show SomeHackageError where
  show (MkSomeHackageError e) = show e
instance Eq SomeHackageError where
  -- hacky, but good enough
  MkSomeHackageError e1 == MkSomeHackageError e2 = show e1 == show e2

renderSkellyError :: SkellyError -> Text
renderSkellyError = \case
  NoPackageConfig locs ->
    Text.unlines $
      "Could not find hspackage.toml in any of the following locations:"
        : ["  - " <> Text.pack loc | loc <- locs]
  ExecutableNotFound exe ->
    "Could not find executable: " <> exe
  SomeHackageError (MkSomeHackageError e) ->
    "Got a Hackage error: " <> Text.pack (displayException e)
  UnknownPackage package ->
    "Unknown package: " <> package
  PackageIdNotFound packageId ->
    "Package not found: " <> renderPackageId packageId
  NoValidVersions package availableVersions versionRange ->
    Text.unlines
      [ "Package " <> package <> " has no versions in the range " <> renderCompiledRange versionRange <> ":"
      , "Available versions: " <> Text.intercalate ", " (map renderVersion availableVersions)
      ]
  BadCabalFile packageId msg ->
    Text.unlines
      [ "Could not parse cabal file for " <> renderPackageId packageId <> ":"
      , msg
      ]
  DependencyResolutionFailure -> "Failed to resolve dependencies"
  UnsatisfiableVersionRange package range ->
    "Unsatisfiable version range for package " <> package <> ": " <> renderVersionRange range
