{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Implements a PackageIndex for Hackage, or a mirror of Hackage.
-}
module Skelly.Core.PackageIndex.Hackage (
  HackageOptions (..),
  initServiceHackage,
  defaultHackageOptions,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Codec.Serialise (Serialise)
import Codec.Serialise qualified as Serialise
import Codec.Serialise.Decoding qualified as Serialise
import Codec.Serialise.Encoding qualified as Serialise
import Control.Monad (join, (>=>))
import Data.ByteString.Lazy qualified as ByteStringL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (URI)
import Skelly.Core.PackageIndex.Interface
import Skelly.Core.Paths (skellyCacheDir)
import Skelly.Core.Types.PackageId (
  PackageId (..),
  PackageName,
  renderPackageId,
 )
import Skelly.Core.Types.Version (
  Version,
  CompiledVersionRange,
  compileRange,
  renderVersionRange,
  wholeRange,
 )
import Skelly.Core.Utils.Cabal qualified as Cabal
import Skelly.Core.Utils.Hackage qualified as Hackage
import System.FilePath ((</>))
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception (throwIO)

data HackageOptions = HackageOptions
  { hackageURI :: URI
  , hackageKeys :: [Text]
  , hackageKeyThreshold :: Int
  }

defaultHackageOptions :: HackageOptions
defaultHackageOptions =
  HackageOptions
    { hackageURI = Hackage.defaultHackageURI
    , hackageKeys = Hackage.defaultHackageKeys
    , hackageKeyThreshold = Hackage.defaultHackageKeyThreshold
    }

initServiceHackage :: Hackage.Service -> HackageOptions -> Service
initServiceHackage service HackageOptions{..} =
  Service
    { withPackageIndex = \f -> Hackage.withRepo service hackageRepoOpts (f . initPackageIndex)
    }
  where
    hackageRepoOpts =
      Hackage.RepoOptions
        { hackageURI
        , hackageKeys
        , hackageKeyThreshold
        , hackageCacheRoot = skellyCacheDir </> "package-index"
        }

initPackageIndex :: Hackage.Repository -> PackageIndex
initPackageIndex repo =
  PackageIndex
    { withIndexCursor = \f -> Hackage.withIndex repo (initPackageIndexCursor repo >=> f)
    , updateMetadata = Hackage.updateMetadata repo
    , downloadPackage = downloadPackage
    }
  where
    downloadPackage pkgId dest = do
      let destTarGz = dest </> Text.unpack (renderPackageId pkgId <> ".tar.gz")
      Hackage.downloadPackageTarGz repo pkgId destTarGz
      Tar.unpack dest . Tar.read . GZip.decompress =<< ByteStringL.readFile destTarGz

initPackageIndexCursor ::
  Hackage.Repository
  -> Hackage.IndexCallbacks
  -> IO PackageIndexCursor
initPackageIndexCursor repo callbacks = do
  ptrs <- syncAndLoadIndexPtrs repo callbacks
  pure
    PackageIndexCursor
      { lookupPackageVersionInfo = \name -> do
          let mAvailableVersions = Map.keys <$> Map.lookup name (packagePtrs ptrs)
          mPreferredVersionRange <- getPreferredVersionRange callbacks ptrs name
          case (mPreferredVersionRange, mAvailableVersions) of
            (Nothing, Nothing) -> pure Nothing
            _ ->
              pure . Just $
                PackageVersionInfo
                  { availableVersions = fromMaybe [] mAvailableVersions
                  , preferredVersionRange = fromMaybe wholeRange mPreferredVersionRange
                  }
      , lookupPackageInfo = \packageId@PackageId{..} ->
          case Map.lookup packageName (packagePtrs ptrs) >>= Map.lookup packageVersion of
            Nothing -> pure Nothing
            Just ptr -> fmap Just $ do
              let Hackage.IndexCallbacks{indexLookupFileEntry} = callbacks
              Hackage.IndexEntry{indexEntryContent} <-
                indexLookupFileEntry ptr (Hackage.IndexPkgCabal $ Cabal.toPackageIdentifier packageId)
              either throwIO pure $ Cabal.parseCabalFile packageId (ByteStringL.toStrict indexEntryContent)
      }

{----- IndexPtrs -----}

-- | Pointers to information in the Index, cacheable to disk.
data IndexPtrs = IndexPtrs
  { nextEntry :: Hackage.DirectoryEntry
  -- ^ Pointer to the next entry to be added to the index
  , packagePtrs :: Map PackageName (Map Version Hackage.DirectoryEntry)
  -- ^ Pointer into the index for the metadata of the given package/version
  , preferredVersionsPtrs :: Map PackageName Hackage.DirectoryEntry
  -- ^ Pointer into the index for the preferred versions for the given package
  }

instance Serialise IndexPtrs where
  encode IndexPtrs{..} =
    mconcat
      [ Serialise.encodeListLen 4
      , Serialise.encodeWord 0
      , Serialise.encode $ Hackage.directoryEntryBlockNo nextEntry
      , Serialise.encode $ (fmap . fmap) Hackage.directoryEntryBlockNo packagePtrs
      , Serialise.encode $ fmap Hackage.directoryEntryBlockNo preferredVersionsPtrs
      ]
  decode = do
    len <- Serialise.decodeListLen
    tag <- Serialise.decodeWord
    case (len, tag) of
      (4, 0) -> do
        nextEntry <- Serialise.decode
        packagePtrs <- Serialise.decode
        preferredVersionsPtrs <- Serialise.decode
        pure
          IndexPtrs
            { nextEntry = Hackage.DirectoryEntry nextEntry
            , packagePtrs = (fmap . fmap) Hackage.DirectoryEntry packagePtrs
            , preferredVersionsPtrs = fmap Hackage.DirectoryEntry preferredVersionsPtrs
            }
      _ -> fail "Invalid IndexPtrs encoding"

syncAndLoadIndexPtrs ::
  Hackage.Repository
  -> Hackage.IndexCallbacks
  -> IO IndexPtrs
syncAndLoadIndexPtrs repo Hackage.IndexCallbacks{..} = do
  root <- Hackage.getRepoRoot repo
  let cachedIndexInfoPath = root </> "skelly-index.cache"

  cachedIndexInfo@IndexPtrs{..} <-
    doesFileExist cachedIndexInfoPath >>= \case
      True -> Serialise.readFileDeserialise cachedIndexInfoPath
      False ->
        pure
          IndexPtrs
            { nextEntry = Hackage.directoryFirst indexDirectory
            , packagePtrs = Map.empty
            , preferredVersionsPtrs = Map.empty
            }

  if nextEntry < Hackage.directoryNext indexDirectory
    then do
      updatedIndexInfo <- updateIndex cachedIndexInfo
      Serialise.writeFileSerialise cachedIndexInfoPath updatedIndexInfo
      pure updatedIndexInfo
    else do
      pure cachedIndexInfo
  where
    updateIndex indexInfo@IndexPtrs{nextEntry = ptr} = do
      (Hackage.Some Hackage.IndexEntry{..}, mNextEntry) <- indexLookupEntry ptr

      -- add entry from index
      let indexInfo' =
            case indexEntryPathParsed of
              Just (Hackage.IndexPkgCabal Cabal.PackageIdentifier{..}) ->
                indexInfo
                  { packagePtrs =
                      initAndAdjust
                        (Map.insert (Cabal.fromCabalVersion pkgVersion) ptr)
                        (Cabal.fromPackageName pkgName)
                        (packagePtrs indexInfo)
                  }
              Just (Hackage.IndexPkgPrefs name) ->
                indexInfo
                  { preferredVersionsPtrs =
                      Map.insert
                        (Cabal.fromPackageName name)
                        ptr
                        (preferredVersionsPtrs indexInfo)
                  }
              Just (Hackage.IndexPkgMetadata _) ->
                indexInfo
              Nothing ->
                indexInfo

      -- continue to the next entry, or exit
      case mNextEntry of
        Just nextEntry -> updateIndex indexInfo'{nextEntry = nextEntry}
        Nothing -> pure indexInfo'{nextEntry = Hackage.directoryNext indexDirectory}

    -- Like 'Map.adjust', except initialize missing keys with 'mempty' before updating.
    initAndAdjust :: (Ord k, Monoid a) => (a -> a) -> k -> Map k a -> Map k a
    initAndAdjust f = Map.alter (Just . f . fromMaybe mempty)

getPreferredVersionRange :: Hackage.IndexCallbacks -> IndexPtrs -> PackageName -> IO (Maybe CompiledVersionRange)
getPreferredVersionRange Hackage.IndexCallbacks{..} IndexPtrs{..} package =
  fmap join . traverse readFromIndex $ Map.lookup package preferredVersionsPtrs
  where
    readFromIndex ptr = do
      Hackage.IndexEntry{indexEntryContent} <-
        indexLookupFileEntry ptr (Hackage.IndexPkgPrefs $ Cabal.toPackageName package)
      case Cabal.parsePreferredVersions package indexEntryContent of
        _ | indexEntryContent == "" -> pure Nothing -- I'm guessing "" means someone removed preferences for a package?
        Just range ->
          case compileRange range of
            Just preferredVersionRange -> pure $ Just preferredVersionRange
            Nothing -> error . Text.unpack $ "Package " <> package <> " has unsatisfiable preferred versions: " <> renderVersionRange range
        Nothing -> error . Text.unpack $ "Could not parse preferred versions for: " <> package
