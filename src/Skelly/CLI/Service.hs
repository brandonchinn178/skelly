{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.Service (
  Service (..),
  Options (..),
  initService,
) where

import Skelly.Core.Logging (LogLevel)
import Skelly.Core.Logging qualified as Logging

-- | The base service passed to all commands.
--
-- Contains common, frequently used services.
data Service = Service
  { loggingService :: Logging.Service
  }

data Options = Options
  { logLevel :: LogLevel
  }

initService :: Options -> Service
initService Options{..} = Service{..}
  where
    loggingService = Logging.initService logLevel
