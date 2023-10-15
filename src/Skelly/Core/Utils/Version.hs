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
  inRange,
  chooseBestVersion,
) where

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

inRange :: VersionRange -> Version -> Bool
inRange = \case
  AnyVersion -> const True
  VersionWithOp VERSION_LT v -> (< v)
  VersionWithOp VERSION_LTE v -> (<= v)
  VersionWithOp VERSION_EQ v -> (== v)
  VersionWithOp VERSION_GT v -> (> v)
  VersionWithOp VERSION_GTE v -> (>= v)
  VersionWithOp VERSION_PVP_MAJOR v -> matchesPVP v
  VersionRangeAnd l r -> inRange l .&&. inRange r
  VersionRangeOr l r -> inRange l .||. inRange r
  where
    (.&&.) = liftA2 (&&)
    (.||.) = liftA2 (||)

    matchesPVP v =
      let (x, y) = toPVP v
       in (>= v) .&&. (< makeVersion [x, y + 1])
    toPVP v =
      case take 2 (Version.versionBranch v) <> repeat 0 of
        x : y : _ -> (x, y)
        l -> error $ "list was unexpectedly not infinite: " ++ show l

chooseBestVersion :: VersionRange -> [Version] -> Maybe Version
chooseBestVersion range = List.find (inRange range) . List.sortOn Down
