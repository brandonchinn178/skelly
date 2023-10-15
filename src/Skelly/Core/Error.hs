{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skelly.Core.Error (
  SkellyError (..),
  renderSkellyError,
) where

import Control.Exception (Exception (..))
import Data.Text (Text)
import Data.Text qualified as Text

data SkellyError
  = NoPackageConfig [FilePath]
  deriving (Show)

instance Exception SkellyError where
  displayException = Text.unpack . renderSkellyError

renderSkellyError :: SkellyError -> Text
renderSkellyError = \case
  NoPackageConfig locs ->
    Text.unlines $
      "Could not find hspackage.toml in any of the following locations:"
        : ["  - " <> Text.pack loc | loc <- locs]
