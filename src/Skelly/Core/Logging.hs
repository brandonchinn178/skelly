{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skelly.Core.Logging (
  -- * Service
  Service (..),
  initService,

  -- * Methods
  logDebug,
  logInfo,
  logWarn,
  logError,

  -- * Low level API
  LogLevel (..),
) where

import Data.Text (Text)
import Data.Text.IO qualified as Text

data Service = Service
  { doLog :: LogLevel -> Text -> IO ()
  }

initService :: LogLevel -> Service
initService minLevel =
  Service
    { doLog = \level msg ->
        if level >= minLevel
          then Text.putStrLn $ "[" <> displayLevel level <> "] " <> msg
          else pure ()
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

logDebug :: Service -> Text -> IO ()
logDebug service = doLog service LevelDebug

logInfo :: Service -> Text -> IO ()
logInfo service = doLog service LevelInfo

logWarn :: Service -> Text -> IO ()
logWarn service = doLog service LevelWarn

logError :: Service -> Text -> IO ()
logError service = doLog service LevelError
