{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Skelly.Core.PackageIndex (
  module Skelly.Core.PackageIndex.Interface,

  -- * Implementations

  -- ** Hackage
  module Skelly.Core.PackageIndex.Hackage,
) where

import Skelly.Core.PackageIndex.Hackage
import Skelly.Core.PackageIndex.Interface
import Skelly.Core.Service (Has, IsService (..), getOpts, loadService)
import Skelly.Core.Utils.Hackage qualified as Utils.Hackage

instance
  ( Has HackageOptions opts
  , IsService opts Utils.Hackage.Service
  ) =>
  IsService opts Service
  where
  initService = do
    hackageOptions <- getOpts
    hackageService <- loadService
    pure $ initServiceHackage hackageService hackageOptions
