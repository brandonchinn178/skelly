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
  renderCompiledRange,
  singletonRange,
  intersectRange,
  isSingletonRange,
  getSingletonRange,
  inRange,
  chooseBestVersion,
  getVersionPreferences,
) where

import Data.Interval (Interval)
import Data.Interval qualified as Interval
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
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
renderVersionRange = go False
  where
    go needsParens = \case
      AnyVersion -> "*"
      VersionWithOp op version -> renderVersionOp op <> renderVersion version
      VersionRangeAnd l r -> parens needsParens $ Text.unwords [go True l, "&&", go True r]
      VersionRangeOr l r -> parens needsParens $ Text.unwords [go True l, "||", go True r]

    parens needsParens =
      if needsParens
        then \s -> "(" <> s <> ")"
        else id

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
newtype CompiledVersionRange =
  CompiledVersionRange
    { unCompiledVersionRange :: NonEmpty (Interval Version)
      -- ^ Range consists of the disjunction of multiple intervals
      --
      -- Invariant: Interval is never empty
    }
  deriving (Show, Eq)

compileRange :: VersionRange -> Maybe CompiledVersionRange
compileRange = fmap simplifyRange . go
  where
    merge (CompiledVersionRange l) (CompiledVersionRange r) = CompiledVersionRange (l <> r)
    range = pure . CompiledVersionRange . NonEmpty.singleton

    go = \case
      AnyVersion -> range Interval.whole
      VersionWithOp VERSION_LT v -> range $ Interval.NegInf Interval.<..< Interval.Finite v
      VersionWithOp VERSION_LTE v -> range $ Interval.NegInf Interval.<..<= Interval.Finite v
      VersionWithOp VERSION_NEQ v ->
        merge
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
      VersionRangeOr l r -> merge <$> go l <*> go r

    nextPvpVersion v =
      case take 2 (Version.versionBranch v) <> repeat 0 of
        x : y : _ -> makeVersion [x, y + 1]
        l -> error $ "list was unexpectedly not infinite: " ++ show l

-- | Return the intersection of the two ranges, or Nothing if the ranges are incompatible.
intersectRange :: CompiledVersionRange -> CompiledVersionRange -> Maybe CompiledVersionRange
intersectRange (CompiledVersionRange l) (CompiledVersionRange r) =
  fmap CompiledVersionRange . NonEmpty.nonEmpty $
    filter (not . Interval.null) $
      [ i1 `Interval.intersection` i2
      | i1 <- NonEmpty.toList l
      , i2 <- NonEmpty.toList r
      ]

simplifyRange :: CompiledVersionRange -> CompiledVersionRange
simplifyRange = CompiledVersionRange . foldNE simplify . sort . unCompiledVersionRange
  where
    sort = NonEmpty.sortOn Interval.lowerBound
    simplify i1 = \case
      i2 : rest | Interval.isConnected i1 i2 -> (Interval.hull i1 i2, rest)
      rest -> (i1, rest)

    foldNE :: (a -> [b] -> (b, [b])) -> NonEmpty a -> NonEmpty b
    foldNE f =
      let go (a NonEmpty.:| as) = f a $ maybe [] (uncurry (:) . go) (NonEmpty.nonEmpty as)
       in uncurry (NonEmpty.:|) . go

renderCompiledRange :: CompiledVersionRange -> Text
renderCompiledRange = renderBoolOps . map renderBounds . NonEmpty.toList . unCompiledVersionRange
  where
    renderBounds i
      | Just v <- Interval.extractSingleton i = ["= " <> renderVersion v]
      | otherwise =
          let fromLo = \case
                (Interval.Finite v, Interval.Open) -> Just $ "> " <> renderVersion v
                (Interval.Finite v, Interval.Closed) -> Just $ "≥ " <> renderVersion v
                _ -> Nothing
              fromHi = \case
                (Interval.Finite v, Interval.Open) -> Just $ "< " <> renderVersion v
                (Interval.Finite v, Interval.Closed) -> Just $ "≤ " <> renderVersion v
                _ -> Nothing
           in catMaybes
                [ fromLo $ Interval.lowerBound' i
                , fromHi $ Interval.upperBound' i
                ]

    -- render ranges as "> 1 && < 2" if there's just one range, otherwise
    -- wrap in parens: "(> 1 && < 2) || (> 3 && < 4)"
    renderBoolOps parts =
      Text.intercalate " || " $
        [ (if needsParens then \s -> "(" <> s <> ")" else id) $
            Text.intercalate " && " bounds
        | bounds <- parts
        , let needsParens = length parts > 1 && length bounds > 1
        ]

-- | A helper for constructing a range matching just the given Version.
singletonRange :: Version -> CompiledVersionRange
singletonRange = CompiledVersionRange . NonEmpty.singleton . Interval.singleton

-- | Return True if the given range contains a single version.
isSingletonRange :: CompiledVersionRange -> Bool
isSingletonRange = isJust . getSingletonRange

-- | Return the version if the given range contains a single version.
getSingletonRange :: CompiledVersionRange -> Maybe Version
getSingletonRange = \case
  CompiledVersionRange (i NonEmpty.:| []) -> Interval.extractSingleton i
  _ -> Nothing

inRange :: CompiledVersionRange -> Version -> Bool
inRange (CompiledVersionRange is) v = any (v `Interval.member`) is

chooseBestVersion :: VersionRange -> [Version] -> Maybe Version
chooseBestVersion range' vs = do
  range <- compileRange range'
  listToMaybe $ getVersionPreferences range vs

-- | Return a list of Versions that match the given range, from most recent to least.
getVersionPreferences :: CompiledVersionRange -> [Version] -> [Version]
getVersionPreferences range = filter (inRange range) . List.sortOn Down
