{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.Service (
  Service (..),
  Options (..),
  initService,
) where

import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Utils.Hackage qualified as Hackage
import Skelly.Core.Utils.HTTP qualified as HTTP

-- | The base service passed to all commands.
--
-- Contains common, frequently used services. Keep the fields ordered
-- by their dependencies; i.e. a service should only depend on services
-- above it.
data Service = Service
  { loggingService :: Logging.Service
  , httpService :: HTTP.Service
  , hackageService :: Hackage.Service
  , packageIndexService :: PackageIndex.Service
  }

data Options = Options
  { logOptions :: Logging.Options
  }

initService :: Options -> IO Service
initService Options{..} = do
  let loggingService = Logging.initService logOptions
  httpService <- HTTP.initService
  let hackageService = Hackage.initService loggingService httpService
  -- TODO: allow user to configure the index provider + hackage opts
  let packageIndexService = PackageIndex.initServiceHackage hackageService PackageIndex.defaultHackageOptions
  pure Service{..}
