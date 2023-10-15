module Skelly.Core.PackageIndex (
  -- * Service
  Service (..),
  PackageIndex (..),
  PackageIndexCursor (..),
  PackageName,
  PackageInfo (..),

  -- * Methods
  getLatestVersion,

  -- * Implementations
  -- ** Hackage
  module Skelly.Core.PackageIndex.Hackage,
) where

import Skelly.Core.PackageIndex.Hackage
import Skelly.Core.PackageIndex.Interface
