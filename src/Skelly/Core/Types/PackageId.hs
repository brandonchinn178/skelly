{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skelly.Core.Types.PackageId (
  PackageName,
  PackageId (..),
  renderPackageId,
  parsePackageId,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Skelly.Core.Types.Version (Version, parseVersion, renderVersion)

type PackageName = Text

data PackageId = PackageId
  { name :: PackageName
  , version :: Version
  }
  deriving (Show, Eq, Ord)

renderPackageId :: PackageId -> Text
renderPackageId PackageId{..} =
  Text.intercalate "-" $
    [ name
    , renderVersion version
    ]

parsePackageId :: Text -> Maybe PackageId
parsePackageId s = do
  let (pre, versionStr) = Text.breakOnEnd "-" s
  let name = Text.dropEnd 1 pre
  version <- parseVersion versionStr
  pure PackageId{..}
