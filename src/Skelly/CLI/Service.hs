{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.Service (
  Service (..),
  Options (..),
  initService,
) where

import Skelly.Core.Logging (LogLevel)
import Skelly.Core.Logging qualified as Logging
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
  }

data Options = Options
  { logLevel :: LogLevel
  }

initService :: Options -> IO Service
initService Options{..} = do
  let loggingService = Logging.initService logLevel
  httpService <- HTTP.initService
  let hackageService = Hackage.initService loggingService httpService
  pure Service{..}
