{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Utils.PackageId (
  PackageId (..),
  renderPackageId,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Skelly.Core.Utils.Version (Version, renderVersion)

data PackageId = PackageId
  { packageName :: Text
  , packageVersion :: Version
  }
  deriving (Show, Eq)

renderPackageId :: PackageId -> Text
renderPackageId PackageId{..} =
  Text.intercalate "-" $
    [ packageName
    , renderVersion packageVersion
    ]
