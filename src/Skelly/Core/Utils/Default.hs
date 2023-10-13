module Skelly.Core.Utils.Default (
  DefaultOptions (..),
) where

-- | Like 'Default' from 'data-default', but with a more descriptive name.
class DefaultOptions a where
  defaultOpts :: a
