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
  decompileRange,
  intersectRange,
  isSingletonRange,
  inRange,
  chooseBestVersion,
  getVersionPreferences,
) where

import Data.Interval (Interval)
import Data.Interval qualified as Interval
import Data.List qualified as List
import Data.Maybe (listToMaybe, mapMaybe)
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
  | VERSION_EQ -- ^ @1.2.3@, matches only the specific version specified
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
      VERSION_EQ -> ""
      VERSION_GT -> "> "
      VERSION_GTE -> ">= "
      VERSION_PVP_MAJOR -> "^"

{----- Compilation -----}

-- | A VersionRange compiled for fast queries.
newtype CompiledVersionRange =
  CompiledVersionRange
    (Interval Version) -- ^ Invariant: Never empty

compileRange :: VersionRange -> CompiledVersionRange
compileRange = CompiledVersionRange . compile
  where
    compile = \case
      AnyVersion -> Interval.whole
      VersionWithOp VERSION_LT v -> Interval.NegInf Interval.<..< Interval.Finite v
      VersionWithOp VERSION_LTE v -> Interval.NegInf Interval.<..<= Interval.Finite v
      VersionWithOp VERSION_EQ v -> Interval.singleton v
      VersionWithOp VERSION_GT v -> Interval.Finite v Interval.<..< Interval.PosInf
      VersionWithOp VERSION_GTE v -> Interval.Finite v Interval.<=..< Interval.PosInf
      VersionWithOp VERSION_PVP_MAJOR v -> Interval.Finite v Interval.<=..< Interval.Finite (nextPvpVersion v)
      VersionRangeAnd l r -> compile l `Interval.intersection` compile r
      VersionRangeOr l r -> compile l `Interval.hull` compile r

    nextPvpVersion v =
      case take 2 (Version.versionBranch v) <> repeat 0 of
        x : y : _ -> makeVersion [x, y + 1]
        l -> error $ "list was unexpectedly not infinite: " ++ show l

decompileRange :: CompiledVersionRange -> VersionRange
decompileRange (CompiledVersionRange i) =
  case (Interval.lowerBound' i, Interval.upperBound' i) of
    ((Interval.NegInf, _), (Interval.NegInf, _)) -> invariantViolation
    ((Interval.NegInf, _), (Interval.Finite v, b)) -> VersionWithOp (toLT b) v
    ((Interval.NegInf, _), (Interval.PosInf, _)) -> AnyVersion
    ((Interval.Finite _, _), (Interval.NegInf, _)) -> invariantViolation
    ((Interval.Finite v1, b1), (Interval.Finite v2, b2)) ->
      case compare v1 v2 of
        LT -> VersionRangeAnd (VersionWithOp (toGT b1) v1) (VersionWithOp (toLT b2) v2)
        EQ -> VersionWithOp VERSION_EQ v1
        GT -> VersionRangeAnd (VersionWithOp (toLT b1) v1) (VersionWithOp (toGT b2) v2)
    ((Interval.Finite v, b), (Interval.PosInf, _)) -> VersionWithOp (toGT b) v
    ((Interval.PosInf, _), (Interval.NegInf, _)) -> invariantViolation
    ((Interval.PosInf, _), (Interval.Finite _, _)) -> invariantViolation
    ((Interval.PosInf, _), (Interval.PosInf, _)) -> invariantViolation
  where
    toGT = \case
      Interval.Open -> VERSION_GT
      Interval.Closed -> VERSION_GTE
    toLT = \case
      Interval.Open -> VERSION_LT
      Interval.Closed -> VERSION_LTE

    -- can't happen, because the interval should never be empty
    invariantViolation = error $ "Compiled version range was unexpectedly empty: " ++ show i

-- | Return the intersection of the two ranges, or Nothing if the ranges are incompatible.
intersectRange :: CompiledVersionRange -> CompiledVersionRange -> Maybe CompiledVersionRange
intersectRange (CompiledVersionRange i1) (CompiledVersionRange i2) =
  let i = i1 `Interval.intersection` i2
   in if Interval.null i then Nothing else Just (CompiledVersionRange i)

-- | Return True if the given range contains a single version.
isSingletonRange :: CompiledVersionRange -> Bool
isSingletonRange (CompiledVersionRange i) = Interval.isSingleton i

inRange :: CompiledVersionRange -> Version -> Bool
inRange (CompiledVersionRange i) v = v `Interval.member` i

chooseBestVersion :: CompiledVersionRange -> [Version] -> Maybe Version
chooseBestVersion range = listToMaybe . getVersionPreferences range

-- | Return a list of Versions that match the given range, from most recent to least.
getVersionPreferences :: CompiledVersionRange -> [Version] -> [Version]
getVersionPreferences range = filter (inRange range) . List.sortOn Down
