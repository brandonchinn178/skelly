{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skelly.Core.Utils.Version (
  -- * Version
  Version,
  makeVersion,
  parseVersion,
  renderVersion,

  -- * VersionRange
  VersionRange (..),
  VersionOp (..),
  parseVersionRange,
  renderVersionRange,

  -- * Range resolution
  CompiledVersionRange,
  compileRange,
  singletonRange,
  intersectRange,
  isSingletonRange,
  getSingletonRange,
  inRange,
  chooseBestVersion,
  getVersionPreferences,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Interval (Interval)
import Data.Interval qualified as Interval
import Data.List qualified as List
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (Version, makeVersion)
import Data.Version qualified as Version
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, (+++))
import Text.ParserCombinators.ReadP qualified as ReadP

runReadP :: ReadP a -> Text -> Maybe a
runReadP m = listToMaybe . mapMaybe getFullMatch . readP_to_S m . Text.unpack
  where
    getFullMatch = \case
      (a, "") -> Just a
      _ -> Nothing

parseVersion :: Text -> Maybe Version
parseVersion = runReadP Version.parseVersion

renderVersion :: Version -> Text
renderVersion = Text.pack . Version.showVersion

data VersionRange
  = AnyVersion
  | VersionWithOp VersionOp Version
  | VersionRangeAnd VersionRange VersionRange
  | VersionRangeOr VersionRange VersionRange
  deriving (Show, Eq)

data VersionOp
  = VERSION_LT -- ^ @< 1.2.3@, matches any version prior to the given version
  | VERSION_LTE -- ^ @<= 1.2.3@, matches any version prior to or equal to the given version
  | VERSION_NEQ -- ^ @!= 1.2.3@, matches any version exception the version specified
  | VERSION_EQ -- ^ @1.2.3@, matches only the exact version specified
  | VERSION_GT -- ^ @> 1.2.3@, matches any version after the given version
  | VERSION_GTE -- ^ @>= 1.2.3@, matches any version after or equal to the given version
  | VERSION_PVP_MAJOR -- ^ @^1.2.3@, matches any version in with the same major version, according to PVP
  deriving (Show, Eq)

parseVersionRange :: Text -> Maybe VersionRange
parseVersionRange = runReadP (parseAny +++ parseRange)
  where
    parseRange = ReadP.chainl1 parseVersionWithOp parseRangeOp

    parseAny = AnyVersion <$ ReadP.string "*"

    parseRangeOp =
      ReadP.choice
        [ VersionRangeAnd <$ token "&&"
        , VersionRangeOr <$ token "||"
        ]

    parseVersionWithOp = do
      op <-
        ReadP.choice
          [ VERSION_LT <$ token "<"
          , VERSION_LTE <$ token "<="
          , VERSION_NEQ <$ token "!="
          , pure VERSION_EQ
          , VERSION_GT <$ token ">"
          , VERSION_GTE <$ token ">="
          , VERSION_PVP_MAJOR <$ token "^"
          ]
      version <- Version.parseVersion <* ReadP.skipSpaces
      pure $ VersionWithOp op version

    token s = ReadP.string s <* ReadP.skipSpaces

renderVersionRange :: VersionRange -> Text
renderVersionRange = \case
  AnyVersion -> "*"
  VersionWithOp op version -> renderVersionOp op <> renderVersion version
  VersionRangeAnd l r -> Text.unwords [renderVersionRange l, "&&", renderVersionRange r]
  VersionRangeOr l r -> Text.unwords [renderVersionRange l, "||", renderVersionRange r]
  where
    renderVersionOp = \case
      VERSION_LT -> "< "
      VERSION_LTE -> "<= "
      VERSION_NEQ -> "!= "
      VERSION_EQ -> ""
      VERSION_GT -> "> "
      VERSION_GTE -> ">= "
      VERSION_PVP_MAJOR -> "^"

{----- Compilation -----}

-- | A VersionRange compiled for fast queries.
data CompiledVersionRange
  = CompiledVersionRange (Interval Version) -- ^ Invariant: Interval is never empty
  | CompiledVersionRangeOr CompiledVersionRange CompiledVersionRange
  deriving (Show, Eq)

compileRange :: VersionRange -> Maybe CompiledVersionRange
compileRange = go
  where
    range = pure . CompiledVersionRange
    go = \case
      AnyVersion -> range Interval.whole
      VersionWithOp VERSION_LT v -> range $ Interval.NegInf Interval.<..< Interval.Finite v
      VersionWithOp VERSION_LTE v -> range $ Interval.NegInf Interval.<..<= Interval.Finite v
      VersionWithOp VERSION_NEQ v ->
        CompiledVersionRangeOr
          <$> go (VersionWithOp VERSION_LT v)
          <*> go (VersionWithOp VERSION_GT v)
      VersionWithOp VERSION_EQ v -> range $ Interval.singleton v
      VersionWithOp VERSION_GT v -> range $ Interval.Finite v Interval.<..< Interval.PosInf
      VersionWithOp VERSION_GTE v -> range $ Interval.Finite v Interval.<=..< Interval.PosInf
      VersionWithOp VERSION_PVP_MAJOR v -> range $ Interval.Finite v Interval.<=..< Interval.Finite (nextPvpVersion v)
      VersionRangeAnd l r -> do
        l' <- go l
        r' <- go r
        intersectRange l' r'
      VersionRangeOr l r -> CompiledVersionRangeOr <$> go l <*> go r

    nextPvpVersion v =
      case take 2 (Version.versionBranch v) <> repeat 0 of
        x : y : _ -> makeVersion [x, y + 1]
        l -> error $ "list was unexpectedly not infinite: " ++ show l

-- | Return the intersection of the two ranges, or Nothing if the ranges are incompatible.
intersectRange :: CompiledVersionRange -> CompiledVersionRange -> Maybe CompiledVersionRange
intersectRange = curry $ \case
  (CompiledVersionRange i1, CompiledVersionRange i2) -> do
    let i = i1 `Interval.intersection` i2
    guard $ (not . Interval.null) i
    pure $ CompiledVersionRange i
  (r, CompiledVersionRangeOr r1 r2) -> distribute r (r1, r2)
  (CompiledVersionRangeOr r1 r2, r) -> distribute r (r1, r2)
  where
    distribute r (r1, r2) =
      case (intersectRange r r1, intersectRange r r2) of
        (Just o1, Just o2) -> pure $ CompiledVersionRangeOr o1 o2
        (o1, o2) -> o1 <|> o2

-- | A helper for constructing a range matching just the given Version.
singletonRange :: Version -> CompiledVersionRange
singletonRange = CompiledVersionRange . Interval.singleton

-- | Return True if the given range contains a single version.
isSingletonRange :: CompiledVersionRange -> Bool
isSingletonRange = isJust . getSingletonRange

-- | Return the version if the given range contains a single version.
getSingletonRange :: CompiledVersionRange -> Maybe Version
getSingletonRange = \case
  CompiledVersionRange i -> Interval.extractSingleton i
  CompiledVersionRangeOr _ _ -> Nothing

inRange :: CompiledVersionRange -> Version -> Bool
inRange = \case
  CompiledVersionRange i -> (`Interval.member` i)
  CompiledVersionRangeOr i1 i2 -> \v -> inRange i1 v || inRange i2 v

chooseBestVersion :: VersionRange -> [Version] -> Maybe Version
chooseBestVersion range' vs = do
  range <- compileRange range'
  listToMaybe $ getVersionPreferences range vs

-- | Return a list of Versions that match the given range, from most recent to least.
getVersionPreferences :: CompiledVersionRange -> [Version] -> [Version]
getVersionPreferences range = filter (inRange range) . List.sortOn Down
