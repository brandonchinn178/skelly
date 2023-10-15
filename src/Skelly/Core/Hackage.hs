module Skelly.Core.Hackage (
  getLatestVersion,
) where

import Data.Text (Text)
import Skelly.Core.Utils.Version (Version)

getLatestVersion :: Text -> IO Version
getLatestVersion = error "TODO"
