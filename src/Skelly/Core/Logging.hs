{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Skelly.Core.Logging (
  -- * Service
  Service (..),
  Options (..),
  initService,

  -- * Methods
  getLogLevel,
  logAt,
  logDebug,
  logInfo,
  logWarn,
  logError,

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
import Skelly.Core.Service (Has, IsService (..), getOpts)

data Service = Service
  { options :: Options
  , doLog :: LogLevel -> Text -> IO ()
  }

data Options = Options
  { logLevel :: LogLevel
  }

instance Has Options opts => IsService opts Service where
  initService = do
    options <- getOpts
    let doLog level msg =
          if level >= logLevel options
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

getLogLevel :: Service -> LogLevel
getLogLevel = logLevel . options

logAt :: Service -> LogLevel -> Text -> IO ()
logAt = doLog

logDebug :: Service -> Text -> IO ()
logDebug service = doLog service LevelDebug

logInfo :: Service -> Text -> IO ()
logInfo service = doLog service LevelInfo

logWarn :: Service -> Text -> IO ()
logWarn service = doLog service LevelWarn

logError :: Service -> Text -> IO ()
logError service = doLog service LevelError

modifyMessage :: (Text -> Text) -> Service -> Service
modifyMessage f service = service{doLog = \lvl msg -> doLog service lvl (f msg)}

prependMessage :: Text -> Service -> Service
prependMessage s = modifyMessage (s <>)

addContext :: Text -> Service -> Service
addContext label = prependMessage $ "[" <> label <> "] "
