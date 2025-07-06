{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Utilities for querying Hackage, or a server running a Hackage mirror.

Fundamentally a wrapper around the hackage-security package.
-}
module Skelly.Core.Utils.Hackage (
  -- * Service
  Service (..),
  initService,

  -- * RepoOptions
  RepoOptions (..),
  defaultHackageURI,
  defaultHackageKeys,
  defaultHackageKeyThreshold,

  -- * Methods
  withRepo,
  withRepoNoBootstrap,
  runBootstrap,
  updateMetadata,
  downloadPackageTarGz,

  -- * Repository
  Repository,
  getRepoRoot,

  -- * Re-exports
  Hackage.Directory (..),
  Hackage.DirectoryEntry (..),
  Hackage.IndexCallbacks (..),
  Hackage.IndexEntry (..),
  Hackage.IndexFile (..),
  Some (..),
  Hackage.withIndex,
) where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as Char8
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Hackage.Security.Client qualified as Hackage
import Hackage.Security.Client.Repository qualified as Hackage
import Hackage.Security.Client.Repository.Cache qualified as Cache
import Hackage.Security.Client.Repository.HttpLib (HttpLib (HttpLib))
import Hackage.Security.Client.Repository.HttpLib qualified as HttpLib
import Hackage.Security.Client.Repository.Remote qualified as Hackage (RemoteTemp)
import Hackage.Security.Client.Repository.Remote qualified as RemoteRepo
import Hackage.Security.Util.Checked (Throws, handleChecked, throwChecked)
import Hackage.Security.Util.Path qualified as Path
import Hackage.Security.Util.Pretty qualified as Pretty
import Hackage.Security.Util.Some (Some (..))
import Network.URI (URI)
import Network.URI qualified as URI
import Network.URI.Static qualified as URI
import Skelly.Core.Error (SkellyError (..), SomeHackageError (..))
import Skelly.Core.Logging (LogLevel (..), logAt, logDebug, logWarn)
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.Types.PackageId (PackageId (..))
import Skelly.Core.Utils.Cabal qualified as Cabal
import Skelly.Core.Utils.HTTP qualified as HTTP
import System.FilePath (takeDirectory, (</>))
import Text.Read (readMaybe)
import UnliftIO.Directory (createDirectoryIfMissing)
import UnliftIO.Exception (Exception, catchAny, displayException, handle, throwIO, tryAny)

{----- Service -----}

data Service = Service
  { loggingService :: Logging.Service
  , httpLib :: HttpLib
  }

initService :: Logging.Service -> HTTP.Service -> Service
initService loggingService httpService = Service{..}
  where
    httpLib = mkHttpLib httpService

{----- Repository options -----}

data RepoOptions = RepoOptions
  { hackageURI :: URI
  , hackageKeys :: [Text]
  , hackageKeyThreshold :: Int
  , hackageCacheRoot :: FilePath
  }

defaultHackageURI :: URI
defaultHackageURI = [URI.uri|https://hackage.haskell.org|]

-- | Source of truth: https://github.com/haskell-infra/hackage-root-keys
defaultHackageKeys :: [Text]
defaultHackageKeys =
  [ -- Adam Gundry (uRPdSiL3/MNsk50z6NB55ABo0OrrNDXigtCul4vtzmw=)
    "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
  , -- Gershom Bazerman (bYoUXXQ9TtX10UriaMiQtTccuXPGnmldP68djzZ7cLo=)
    "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
  , -- John Wiegley (zazm5w480r+zPO6Z0+8fjGuxZtb9pAuoVmQ+VkuCvgU=)
    "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
  , -- Norman Ramsey (ZI8di3a9Un0s2RBrt5GwVRvfOXVuywADfXGPZfkiDb0=)
    "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
  , -- Mathieu Boespflug (ydN1nGGQ79K1Q0nN+ul+Ln8MxikTB95w0YdGd3v3kmg=)
    "be75553f3c7ba1dbe298da81f1d1b05c9d39dd8ed2616c9bddf1525ca8c03e48"
  , -- Joachim Breitner (5iUgwqZCWrCJktqMx0bBMIuoIyT4A1RYGozzchRN9rA=)
    "d26e46f3b631aae1433b89379a6c68bd417eb5d1c408f0643dcc07757fece522"
  ]

defaultHackageKeyThreshold :: Int
defaultHackageKeyThreshold = 3

{----- Repository -----}

type Repository = Hackage.Repository Hackage.RemoteTemp

-- | Provide a Repository
withRepo :: Service -> RepoOptions -> (Repository -> IO a) -> IO a
withRepo service@Service{..} opts f = wrapHackageErrors $
  withRepoNoBootstrap service opts $ \repo -> do
    -- Initialize Hackage repo if running for the first time
    Hackage.requiresBootstrap repo >>= \case
      False -> pure ()
      True -> do
        logDebug loggingService "Bootstrapping chain of trust"
        -- TODO: take bootstrap keys + threshold as input, if user wants to use
        -- their own keys, or if user is using their own Hackage server
        runBootstrap opts repo

        logDebug loggingService "Download Hackage repo for the first time"
        updateMetadata repo

    whenMetadataExpired repo $ do
      updateMetadata repo `catchAny` \e -> do
        logWarn loggingService "Failed to update Hackage metadata, run `skelly cache update` manually."
        logDebug loggingService $ (Text.pack . displayException) e

    f repo

-- | Same as 'withRepo', but without running any bootstrapping steps.
--
-- Operations will fail if bootstrapping has not been done, so prefer 'withRepo'.
withRepoNoBootstrap :: Service -> RepoOptions -> (Repository -> IO a) -> IO a
withRepoNoBootstrap Service{..} RepoOptions{..} =
  RemoteRepo.withRepository
    httpLib
    [hackageURI]
    RemoteRepo.defaultRepoOpts
    hackageCache
    Hackage.hackageRepoLayout
    Hackage.hackageIndexLayout
    (mkHackageLogger loggingService)
  where
    hackageHost = URI.uriRegName . fromMaybe URI.nullURIAuth . URI.uriAuthority $ hackageURI
    hackageCache =
      Cache.Cache
        { cacheRoot = Path.fromAbsoluteFilePath hackageCacheRoot Path.</> Path.fragment hackageHost
        , cacheLayout =
            Hackage.CacheLayout
              { cacheLayoutRoot = relPath "root.json"
              , cacheLayoutTimestamp = relPath "timestamp.json"
              , cacheLayoutSnapshot = relPath "snapshot.json"
              , cacheLayoutMirrors = relPath "mirrors.json"
              , cacheLayoutIndexTar = relPath "01-index.tar"
              , cacheLayoutIndexIdx = relPath "01-index.tar.idx"
              , cacheLayoutIndexTarGz = relPath "01-index.tar.gz"
              }
        }

    relPath :: FilePath -> Path.Path root
    relPath = Path.rootPath . Path.fragment

runBootstrap :: RepoOptions -> Repository -> IO ()
runBootstrap RepoOptions{..} repo = wrapHackageErrors $
  Hackage.bootstrap
    repo
    (map (Hackage.KeyId . Text.unpack) hackageKeys)
    (Hackage.KeyThreshold $ fromIntegral hackageKeyThreshold)

updateMetadata :: Repository -> IO ()
updateMetadata repo = wrapHackageErrors $ do
  now <- getCurrentTime
  _ <- Hackage.checkForUpdates repo (Just now)
  pure ()

whenMetadataExpired :: Repository -> IO () -> IO ()
whenMetadataExpired repo action = do
  root <- getRepoRoot repo
  let metadataLastCheck = root </> "skelly-metadata-last-check"

  now <- getCurrentTime
  expired <- isExpired metadataLastCheck now
  when expired $ do
    action
    Text.writeFile metadataLastCheck (Text.pack $ show now)
  where
    expiry = 3600 * 24 :: NominalDiffTime -- update metadata once per day
    isExpired metadataLastCheck now =
      tryAny (Text.readFile metadataLastCheck) >>= \case
        Right content
          | Just lastCheck <- readMaybe $ Text.unpack content
          , lastCheck > addUTCTime (-1 * expiry) now ->
              pure False
        _ -> pure True

downloadPackageTarGz :: Repository -> PackageId -> FilePath -> IO ()
downloadPackageTarGz repo packageId dest = wrapHackageErrors $ do
  createDirectoryIfMissing True $ takeDirectory dest
  Hackage.downloadPackage' repo (Cabal.toPackageIdentifier packageId) dest

-- | Get the directory containing root.json.
getRepoRoot :: Repository -> IO FilePath
getRepoRoot repo = Path.toFilePath . Path.takeDirectory <$> Hackage.repGetCachedRoot repo

{----- Helpers -----}

mkHttpLib :: HTTP.Service -> HttpLib
mkHttpLib httpService =
  HttpLib
    { httpGet = \reqHeaders uri callback -> wrapErrors $ do
        req <- mkRequest uri $ fromRequestHeaders reqHeaders
        withResponseChecked req $ \resp ->
          callback
            (toResponseHeaders $ HTTP.responseHeaders resp)
            (HTTP.responseBody resp)
    , httpGetRange = \reqHeaders uri range callback -> wrapErrors $ do
        req <- mkRequest uri $ (HTTP.hRange, mkRangeHeader range) : fromRequestHeaders reqHeaders
        withResponseChecked req $ \resp -> do
          status <-
            case toResponseStatus (HTTP.responseStatus resp) of
              Just status -> pure status
              Nothing -> do
                body <- HTTP.brRead (HTTP.responseBody resp)
                throwIO $
                  HTTP.HttpExceptionRequest req $
                    HTTP.StatusCodeException (() <$ resp) body
          callback
            status
            (toResponseHeaders $ HTTP.responseHeaders resp)
            (HTTP.responseBody resp)
    }
  where
    wrapErrors :: (Throws Hackage.SomeRemoteError) => IO a -> IO a
    wrapErrors = handle (throwChecked . Hackage.SomeRemoteError @HTTP.HttpException)

    mkRequest uri headers = do
      req <- HTTP.requestFromURI uri
      pure
        . HTTP.setRequestCheckStatus
        $ req { HTTP.requestHeaders = headers }

    fromRequestHeaders headers =
      flip map headers $ \case
        HttpLib.HttpRequestMaxAge0 -> (HTTP.hCacheControl, "max-age=0")
        HttpLib.HttpRequestNoTransform -> (HTTP.hCacheControl, "no-transform")

    toResponseHeaders headers =
      catMaybes
        [ case lookup HTTP.hAcceptRanges headers of
            Just "bytes" -> Just HttpLib.HttpResponseAcceptRangesBytes
            _ -> Nothing
        ]

    toResponseStatus status =
      case HTTP.statusCode status of
        200 -> Just HttpLib.HttpStatus200OK
        206 -> Just HttpLib.HttpStatus206PartialContent
        _ -> Nothing

    withResponseChecked ::
      (Throws Hackage.SomeRemoteError) =>
      HTTP.Request
      -> (HTTP.Response HTTP.BodyReader -> IO a)
      -> IO a
    withResponseChecked req f =
      HTTP.withResponse httpService req f >>= \case
        Left e -> throwChecked $ Hackage.SomeRemoteError e
        Right x -> pure x

    -- Accept bounds with exclusive upper bound.
    -- Content-Range header uses inclusive rather than exclusive bounds.
    -- See <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html>
    mkRangeHeader (from, upTo) = Char8.pack $ "bytes=" <> show from <> "-" <> show (upTo - 1)

mkHackageLogger :: Logging.Service -> Hackage.LogMessage -> IO ()
mkHackageLogger loggingService msg =
  logAt loggingService (toLogLevel msg) $
    "[Hackage] " <> Text.pack (Pretty.pretty msg)
  where
    toLogLevel = \case
      Hackage.LogRootUpdated{} -> LevelDebug
      Hackage.LogVerificationError{} -> LevelError
      Hackage.LogDownloading{} -> LevelDebug
      Hackage.LogUpdating{} -> LevelDebug
      Hackage.LogSelectedMirror{} -> LevelDebug
      Hackage.LogCannotUpdate{} -> LevelError
      Hackage.LogMirrorFailed{} -> LevelError
      Hackage.LogLockWait{} -> LevelDebug
      Hackage.LogLockWaitDone{} -> LevelDebug
      Hackage.LogUnlock{} -> LevelDebug

wrapHackageErrors ::
  ( ( Throws Hackage.VerificationError
    , Throws Hackage.SomeRemoteError
    , Throws Hackage.InvalidPackageException
    ) =>
    IO a
  ) ->
  IO a
wrapHackageErrors m =
  id
    $ wrap @Hackage.VerificationError
    $ wrap @Hackage.SomeRemoteError
    $ wrap @Hackage.InvalidPackageException
    $ m
  where
    wrap :: forall e a. Exception e => (Throws e => IO a) -> IO a
    wrap = handleChecked (throwIO . SomeHackageError . MkSomeHackageError @e)
