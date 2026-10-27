{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skelly.Core.Types.Version (
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
  prettyCompiledRange,
  singletonRange,
  wholeRange,
  intersectRange,
  isEmptyRange,
  isSingletonRange,
  getSingletonRange,
  inRange,
) where

import Control.Monad.Combinators.Expr qualified as Combinators
import Data.Aeson qualified as Aeson
import Data.Interval (Interval)
import Data.Interval qualified as Interval
import Data.List (sortOn)
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
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

instance Aeson.ToJSON VersionRange where
  toJSON = Aeson.toJSON . renderVersionRange
instance Aeson.FromJSON VersionRange where
  parseJSON = Aeson.withText "VersionRange" $ \s ->
    maybe (fail "Invalid VersionRange") pure $
      parseVersionRange s

data VersionOp
  = -- | @< 1.2.3@, matches any version prior to the given version
    VERSION_LT
  | -- | @<= 1.2.3@, matches any version prior to or equal to the given version
    VERSION_LTE
  | -- | @!= 1.2.3@, matches any version exception the version specified
    VERSION_NEQ
  | -- | @1.2.3@, matches only the exact version specified
    VERSION_EQ
  | -- | @> 1.2.3@, matches any version after the given version
    VERSION_GT
  | -- | @>= 1.2.3@, matches any version after or equal to the given version
    VERSION_GTE
  | -- | @^1.2.3@, matches any version in with the same major version, according to PVP
    VERSION_PVP_MAJOR
  deriving (Show, Eq)

parseVersionRange :: Text -> Maybe VersionRange
parseVersionRange = runReadP (parseAny +++ parseRange)
 where
  parseRange =
    Combinators.makeExprParser
      ( ReadP.choice
          [ ReadP.between (token "(") (token ")") parseRange
          , parseVersionWithOp
          ]
      )
      [
        [ Combinators.InfixL (VersionRangeAnd <$ token "&&")
        , Combinators.InfixL (VersionRangeOr <$ token "||")
        ]
      ]

  parseAny = AnyVersion <$ ReadP.string "*"

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
newtype CompiledVersionRange = CompiledVersionRange
  { unwrap :: [Interval Version]
  -- ^ Range consists of the disjunction of multiple intervals
  }
  deriving newtype (Show, Eq, Semigroup, Monoid)

compileRange :: VersionRange -> CompiledVersionRange
compileRange = simplifyRange . go
 where
  range x = CompiledVersionRange [x]

  go = \case
    AnyVersion -> range Interval.whole
    VersionWithOp VERSION_LT v -> range $ Interval.NegInf Interval.<..< Interval.Finite v
    VersionWithOp VERSION_LTE v -> range $ Interval.NegInf Interval.<..<= Interval.Finite v
    VersionWithOp VERSION_NEQ v -> go (VersionWithOp VERSION_LT v) <> go (VersionWithOp VERSION_GT v)
    VersionWithOp VERSION_EQ v -> range $ Interval.singleton v
    VersionWithOp VERSION_GT v -> range $ Interval.Finite v Interval.<..< Interval.PosInf
    VersionWithOp VERSION_GTE v -> range $ Interval.Finite v Interval.<=..< Interval.PosInf
    VersionWithOp VERSION_PVP_MAJOR v -> range $ Interval.Finite v Interval.<=..< Interval.Finite (nextPvpVersion v)
    VersionRangeAnd l r -> intersectRange (go l) (go r)
    VersionRangeOr l r -> go l <> go r

  nextPvpVersion v =
    case take 2 (Version.versionBranch v) <> repeat 0 of
      x : y : _ -> makeVersion [x, y + 1]
      l -> error $ "list was unexpectedly not infinite: " ++ show l

-- | Return the intersection of the two ranges
intersectRange :: CompiledVersionRange -> CompiledVersionRange -> CompiledVersionRange
intersectRange (CompiledVersionRange l) (CompiledVersionRange r) =
  simplifyRange . CompiledVersionRange $
    [ i1 `Interval.intersection` i2
    | i1 <- l
    , i2 <- r
    ]

simplifyRange :: CompiledVersionRange -> CompiledVersionRange
simplifyRange = CompiledVersionRange . simplify . sort . filter notEmpty . (.unwrap)
 where
  sort = sortOn Interval.lowerBound
  notEmpty = not . Interval.null
  simplify = \case
    [] -> []
    i : [] -> [i]
    i1 : i2 : rest ->
      if Interval.isConnected i1 i2
        then simplify (Interval.hull i1 i2 : rest)
        else i1 : simplify (i2 : rest)

prettyCompiledRange :: CompiledVersionRange -> Text
prettyCompiledRange = renderBoolOps . map renderBounds . (.unwrap)
 where
  renderBounds i
    | Just v <- Interval.extractSingleton i = ["= " <> renderVersion v]
    | otherwise =
        let fromLo = \case
              (Interval.Finite v, Interval.Open) -> Just $ "> " <> renderVersion v
              (Interval.Finite v, Interval.Closed) -> Just $ gteSymbol <> " " <> renderVersion v
              _ -> Nothing
            fromHi = \case
              (Interval.Finite v, Interval.Open) -> Just $ "< " <> renderVersion v
              (Interval.Finite v, Interval.Closed) -> Just $ lteSymbol <> " " <> renderVersion v
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

  gteSymbol = "≥"
  lteSymbol = "≤"

-- | A helper for constructing a range matching just the given Version.
singletonRange :: Version -> CompiledVersionRange
singletonRange v = CompiledVersionRange [Interval.singleton v]

-- | A helper for a range matching any version
wholeRange :: CompiledVersionRange
wholeRange = CompiledVersionRange [Interval.whole]

-- | Return True if the given range is empty.
isEmptyRange :: CompiledVersionRange -> Bool
isEmptyRange = null . (.unwrap)

-- | Return True if the given range contains a single version.
isSingletonRange :: CompiledVersionRange -> Bool
isSingletonRange = isJust . getSingletonRange

-- | Return the version if the given range contains a single version.
getSingletonRange :: CompiledVersionRange -> Maybe Version
getSingletonRange = \case
  CompiledVersionRange [i] -> Interval.extractSingleton i
  _ -> Nothing

inRange :: CompiledVersionRange -> Version -> Bool
inRange (CompiledVersionRange is) v = any (v `Interval.member`) is
