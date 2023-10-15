{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | TODO: Break this out into a new `toml-exact` library
module Skelly.Core.Utils.TOML (
  -- * Types
  Document,
  TOML.Value (..),
  TOML.Table,

  -- * Decode/Encode
  decode,
  decodeFile,
  encode,
  encodeFile,

  -- * Methods
  getTable,
  setKey,
  parseWith,

  -- * Parsing
  TOML.Decoder,
  TOML.getField,
  TOML.getFields,
  TOML.getFieldWith,
  TOML.getFieldsWith,
  TOML.makeDecoder,
  TOML.runDecoder,
  TOML.invalidValue,
  TOML.typeMismatch,

  -- * Errors
  TOML.TOMLError (..),
  TOML.NormalizeError (..),
  TOML.DecodeError (..),
  TOML.renderTOMLError,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Foldable qualified as Seq (toList)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Time (timeZoneOffsetString)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Skelly.Core.Paths (skellyCacheDir)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import TOML qualified
import TOML.Decode qualified as TOML
import UnliftIO.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  findExecutable,
 )

-- | TODO: Implement our own type instead of reusing `toml-reader`, so
-- we can modify and write it back out, preserving style.
data Document = Document
  { parsedTable :: TOML.Table
  -- ^ The parsed TOML document.
  --
  -- Modification outside of provided functions is not allowed, as
  -- this needs to stay in sync with rawDoc and docUpdates.
  --
  -- Invariant: parsedTable = rawDoc + docUpdates
  , rawDoc :: Text
  -- ^ The raw document initially parsed.
  , docUpdates :: Seq DocumentUpdate
  -- ^ Updates to the raw document, reflected in parsedTable.
  }

-- | Decode a TOML Document from the given text.
decode :: Text -> Either TOML.TOMLError Document
decode s = do
  table <- TOML.decode s
  pure
    Document
      { parsedTable = table
      , rawDoc = s
      , docUpdates = Seq.empty
      }

decodeFile :: FilePath -> IO (Either TOML.TOMLError Document)
decodeFile = fmap decode . Text.readFile

getTable :: Document -> TOML.Table
getTable = parsedTable

{----- Modification -----}

data DocumentUpdate = SetKey [Text] TOML.Value

-- | Encode a TOML Document to text.
--
-- For prototype purposes, requires Python 3 installed.
-- TODO: Implement natively in Haskell
encode :: Document -> Text
encode Document{..} = unsafePerformIO $ do
  let cacheDir = skellyCacheDir </> "toml-encode-python"
  createDirectoryIfMissing True cacheDir

  let venvDir = cacheDir </> "venv"
  doesDirectoryExist venvDir >>= \case
    True -> pure ()
    False -> do
      pythonExe <- findExecutable "python3" >>= \case
        Just exe -> pure exe
        Nothing -> error "Could not find Python 3 -- Skelly requires Python 3 for now"
      run_ pythonExe ["-m", "venv", venvDir]
      run_ (venvDir </> "bin" </> "pip") ["install", "tomlkit"]

  let script = cacheDir </> "modify.py"
  writeFile script $(embedStringFile "src/Skelly/Core/Utils/TOML_encode.py")

  Text.pack <$> run (venvDir </> "bin" </> "python3") [script, updates] (Text.unpack rawDoc)
  where
    run_ cmd args = run cmd args "" >> pure ()
    run cmd args stdin = do
      (code, stdout, stderr) <- readProcessWithExitCode cmd args stdin
      case code of
        ExitSuccess{} -> pure stdout
        ExitFailure{} ->
          error . unlines $
            [ "Could not encode TOML document:"
            , stdout
            , stderr
            ]

    updates =
      Text.unpack . TextL.toStrict . Aeson.encodeToLazyText $
        [ case update of
            SetKey keys val ->
              Aeson.object
                [ "type" Aeson..= ("set-key" :: Text)
                , "keys" Aeson..= keys
                , "val" Aeson..= tomlToJSON val
                ]
        | update <- Seq.toList docUpdates
        ]
    tomlToJSON = \case
      TOML.Table table -> Aeson.toJSON $ tomlToJSON <$> table
      TOML.Array vs -> Aeson.toJSON $ map tomlToJSON vs
      TOML.String s -> Aeson.toJSON s
      TOML.Integer x -> Aeson.toJSON x
      TOML.Float x -> Aeson.toJSON x
      TOML.Boolean b -> Aeson.toJSON b
      TOML.OffsetDateTime (dt, tz) -> Aeson.toJSON (dt, timeZoneOffsetString tz)
      TOML.LocalDateTime dt -> Aeson.toJSON dt
      TOML.LocalDate d -> Aeson.toJSON d
      TOML.LocalTime t -> Aeson.toJSON t

encodeFile :: FilePath -> Document -> IO ()
encodeFile fp = Text.writeFile fp . encode

-- | Set a possibly nested key to the given value.
--
-- If it's not possible to traverse (e.g. you specify a key that
-- already exists and is not a table), returns document unmodified.
setKey :: [Text] -> TOML.Value -> Document -> Document
setKey keys val doc@Document{..} =
  case setInTable parsedTable keys of
    Just parsedTable' ->
      doc
        { parsedTable = parsedTable'
        , docUpdates = docUpdates <> Seq.singleton (SetKey keys val)
        }
    Nothing -> doc
  where
    setInTable tab = \case
      [] -> Nothing
      k : [] -> pure $ Map.insert k val tab
      k : ks -> do
        nextTable <-
          case Map.lookup k tab of
            Nothing -> pure Map.empty
            Just (TOML.Table tab') -> pure tab'
            Just _ -> Nothing
        setInTable nextTable ks

{---- Parsing ----}

parseWith :: TOML.Decoder a -> Document -> Either TOML.TOMLError a
parseWith decoder Document{parsedTable} =
  runDecodeM $
    TOML.runDecoder decoder (TOML.Table parsedTable)
  where
    runDecodeM (TOML.DecodeM run) = first (uncurry TOML.DecodeError) $ run []
