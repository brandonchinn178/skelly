{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skelly.Core.Logging (
  -- * Service
  Service (..),
  Options (..),
  initService,

  -- ** Modifiers
  modifyMessage,
  prependMessage,
  addContext,

  -- * Low level API
  LogLevel (..),

  -- * Test helpers
  disabledService,
) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import Skelly.Core.Service (Has, IsService (..), getOpts)

data Service = Service
  { options :: Options
  , doLog :: LogLevel -> Text -> IO ()
  }

data Options = Options
  { logLevel :: LogLevel
  }

instance (Has Options opts) => IsService opts Service where
  initService = do
    options <- getOpts
    let doLog level msg =
          if level >= options.logLevel
            then Text.putStrLn $ "[" <> displayLevel level <> "] " <> msg
            else pure ()
    pure Service{..}

disabledService :: Service
disabledService =
  Service
    { options =
        Options
          { logLevel = LevelDebug
          }
    , doLog = \_ _ -> pure ()
    }

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Show, Eq, Ord)

displayLevel :: LogLevel -> Text
displayLevel = \case
  LevelDebug -> "DEBUG"
  LevelInfo -> "INFO"
  LevelWarn -> "WARN"
  LevelError -> "ERROR"

instance HasField "logLevel" Service LogLevel where
  getField = (.options.logLevel)
instance HasField "log" Service (LogLevel -> Text -> IO ()) where
  getField = (.doLog)
instance HasField "debug" Service (Text -> IO ()) where
  getField service = service.log LevelDebug
instance HasField "info" Service (Text -> IO ()) where
  getField service = service.log LevelInfo
instance HasField "warn" Service (Text -> IO ()) where
  getField service = service.log LevelWarn
instance HasField "error" Service (Text -> IO ()) where
  getField service = service.log LevelError

modifyMessage :: (Text -> Text) -> Service -> Service
modifyMessage f service = service{doLog = \lvl msg -> service.log lvl (f msg)}

prependMessage :: Text -> Service -> Service
prependMessage s = modifyMessage (s <>)

addContext :: Text -> Service -> Service
addContext label = prependMessage $ "[" <> label <> "] "
