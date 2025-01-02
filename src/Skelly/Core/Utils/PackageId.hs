{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Utils.PackageId (
  PackageName,
  PackageId (..),
  renderPackageId,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Skelly.Core.Utils.Version (Version, renderVersion)

type PackageName = Text

data PackageId = PackageId
  { packageName :: PackageName
  , packageVersion :: Version
  }
  deriving (Show, Eq, Ord)

renderPackageId :: PackageId -> Text
renderPackageId PackageId{..} =
  Text.intercalate "-" $
    [ packageName
    , renderVersion packageVersion
    ]
