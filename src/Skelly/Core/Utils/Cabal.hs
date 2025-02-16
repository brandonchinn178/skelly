{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Skelly.Core.Utils.Cabal (
  parsePreferredVersions,

  -- * Package info
  PackageInfo (..),
  parseCabalFile,

  -- * Package names
  Cabal.PackageIdentifier (..),
  Cabal.mkPackageName,
  fromPackageName,
  toPackageName,
  toPackageIdentifier,

  -- * Versions
  fromCabalVersion,
  toCabalVersion,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version qualified as Version
import Distribution.Client.IndexUtils qualified as Cabal
import Distribution.Compiler qualified as Cabal
import Distribution.PackageDescription qualified as Cabal
import Distribution.PackageDescription.Configuration qualified as Cabal
import Distribution.PackageDescription.Parsec qualified as Cabal
import Distribution.Parsec.Error qualified as Cabal
import Distribution.Pretty qualified as Cabal
import Distribution.System qualified as Cabal
import Distribution.Types.ComponentRequestedSpec qualified as Cabal
import Distribution.Types.Version qualified as Cabal
import Distribution.Types.VersionRange qualified as Cabal
import Distribution.Utils.Path qualified as Cabal
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Types.PackageId (PackageId (..), renderPackageId)
import Skelly.Core.Types.Version (
  Version,
  VersionOp (..),
  VersionRange (..),
  makeVersion,
 )

-- | Get the preferred versions for the given package
-- in the given 'preferred-versions' file.
parsePreferredVersions :: Text -> Lazy.ByteString -> Maybe VersionRange
parsePreferredVersions package =
  listToMaybe
    . mapMaybe go
    . Cabal.parsePreferredVersionsWarnings
  where
    go = \case
      Right (Cabal.Dependency pkg range _)
        | Cabal.unPackageName pkg == Text.unpack package ->
            Just $ fromCabalVersionRange range
      _ -> Nothing

data PackageInfo = PackageInfo
  { packageSrcDirs :: [FilePath]
  , packageDependencies :: Map Text VersionRange
  , packageDefaultExtensions :: [Text]
  }

parseCabalFile :: PackageId -> ByteString -> Either SkellyError PackageInfo
parseCabalFile packageId input = do
  -- ignore warnings for now
  let (_warnings, result) = Cabal.runParseResult $ Cabal.parseGenericPackageDescription input

  pkg <-
    case result of
      Right pd ->
        case resolveConditions pd of
          Right (pkg, _) -> pure pkg
          Left _ ->
            -- TODO
            error $ "Could not resolve conditions: " ++ show pd
      Left (_cabalVersion, errors) ->
        Left . BadCabalFile packageId $
          Text.unlines . map renderPError . NonEmpty.toList $ errors
  let lib = fromMaybe Cabal.emptyLibrary (Cabal.library pkg)

  pure
    PackageInfo
      { packageSrcDirs = map Cabal.getSymbolicPath . Cabal.hsSourceDirs . Cabal.libBuildInfo $ lib
      , packageDependencies =
          Map.fromList
            [ (fromPackageName name, fromCabalVersionRange range)
            | Cabal.Dependency name range _ <- (Cabal.targetBuildDepends . Cabal.libBuildInfo) lib
            ]
      , packageDefaultExtensions =
          concat
            [ map (Text.pack . Cabal.prettyShow) . Cabal.defaultExtensions . Cabal.libBuildInfo $ lib
            , map (Text.pack . Cabal.prettyShow) . maybeToList . Cabal.defaultLanguage . Cabal.libBuildInfo $ lib
            ]
      }
  where
    renderPError = Text.pack . Cabal.showPError (Text.unpack $ renderPackageId packageId)

    resolveConditions =
      Cabal.finalizePD
        mempty
        Cabal.ComponentRequestedSpec
          { testsRequested = False
          , benchmarksRequested = False
          }
        (const True) -- FIXME: get current solver state (unix-2.8.5.1 + directory needs to set os-string to true)
        platform
        compilerInfo
        []

    -- TODO: read "Target platform" from `ghc --info` (store in CompilerEnv)
    platform = Cabal.buildPlatform

    -- TODO: get actual GHC version from CompilerEnv
    compilerInfo =
      Cabal.CompilerInfo
        { compilerInfoId = Cabal.CompilerId Cabal.GHC (Cabal.mkVersion [9, 10, 1])
        , compilerInfoAbiTag = Cabal.NoAbiTag
        , compilerInfoCompat = Nothing
        , compilerInfoLanguages = Nothing
        , compilerInfoExtensions = Nothing
        }

fromCabalVersion :: Cabal.Version -> Version
fromCabalVersion = makeVersion . Cabal.versionNumbers

toCabalVersion :: Version -> Cabal.Version
toCabalVersion = Cabal.mkVersion . Version.versionBranch

fromCabalVersionRange :: Cabal.VersionRange -> VersionRange
fromCabalVersionRange =
  Cabal.foldVersionRange
    AnyVersion
    (VersionWithOp VERSION_EQ . fromCabalVersion)
    (VersionWithOp VERSION_GT . fromCabalVersion)
    (VersionWithOp VERSION_LT . fromCabalVersion)
    VersionRangeOr
    VersionRangeAnd

fromPackageName :: Cabal.PackageName -> Text
fromPackageName = Text.pack . Cabal.unPackageName

toPackageName :: Text -> Cabal.PackageName
toPackageName = Cabal.mkPackageName . Text.unpack

toPackageIdentifier :: PackageId -> Cabal.PackageIdentifier
toPackageIdentifier PackageId{..} =
  Cabal.PackageIdentifier
    { Cabal.pkgName = toPackageName packageName
    , Cabal.pkgVersion = toCabalVersion packageVersion
    }
