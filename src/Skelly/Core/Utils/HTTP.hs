{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Utils.HTTP (
  -- * Service
  Service (..),
  initService,

  -- * Methods
  withResponse,

  -- * Re-exports
  module X,
) where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client as X hiding (withResponse)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types as X
import Network.HTTP.Types.Header as X
import Skelly.Core.Service (IsService (..))
import UnliftIO.Exception (try)

data Service = Service
  { manager :: Manager
  }

instance IsService opts Service where
  initService = do
    manager <- liftIO newTlsManager
    pure Service{..}

withResponse :: Service -> Request -> (Response BodyReader -> IO a) -> IO (Either HttpException a)
withResponse Service{..} req = try . HTTP.withResponse req manager
