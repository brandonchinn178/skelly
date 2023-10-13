{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Skelly.Core.Build (
  -- * Service
  Service (..),
  initService,

  -- * Methods
  run,
  Options (..),
  allOptionTargets,
  Targets (..),
) where

import Control.Monad (forM, forM_, unless)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Graph qualified as Graph
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Tuple (swap)
import Skelly.Core.Error (SkellyError (..))
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageConfig (PackageConfig)
import Skelly.Core.PackageConfig qualified as PackageConfig
import Skelly.Core.PackageDB (
  PackageDB,
  loadPackageDB,
  packageDbEntries,
  packageDbPath,
  registerPackageInDB,
 )
import Skelly.Core.Parse (parseImports)
import Skelly.Core.Paths (packageDistDir)
import Skelly.Core.Utils.Default (defaultOpts)
import Skelly.Core.Utils.InstalledPackageInfo (InstalledPackageInfo (..))
import Skelly.Core.Utils.Modules (
  ModuleName (ModuleName),
  ModuleNameId (unModuleNameId),
  isMainModule,
  parseModulePath,
  renderModuleName,
 )
import Skelly.Core.Utils.PackageId (PackageId (PackageId), renderPackageId)
import Skelly.Core.Utils.PackageId qualified as PackageId
import Skelly.Core.Utils.Path (listFiles)
import Skelly.Core.Utils.Version (makeVersion)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess)
import UnliftIO.Async (pooledForConcurrently)
import UnliftIO.Exception (throwIO)

data Service = Service
  { loggingService :: Logging.Service
  , loadPackageConfig :: IO PackageConfig
  }

initService :: Logging.Service -> Service
initService loggingService =
  Service
    { loggingService
    , loadPackageConfig = PackageConfig.loadPackageConfig loggingService
    }

{----- Options -----}

data Options = Options
  { libTargets :: Targets
  , binTargets :: Targets
  , testTargets :: Targets
  }
  deriving (Show)

allOptionTargets :: Options -> [Targets]
allOptionTargets Options{..} =
  [ libTargets
  , binTargets
  , testTargets
  ]

{----- Targets -----}

-- TODO: support globs?
type Target = Text

data Targets
  = AllTargets
  | Targets [Target]
  deriving (Show, Eq)

resolveTargets :: Map Text a -> Targets -> (Map Text a, [Target])
resolveTargets components = first collect . mapEither resolveTarget . toTargetsList
  where
    toTargetsList = \case
      AllTargets -> Map.keys components
      Targets targets -> targets
    resolveTarget target =
      -- TODO: resolve target as pattern, instead of exact match
      if target `Map.member` components
        then Right [target]
        else Left target
    collect = Map.restrictKeys components . Set.fromList . concat

    mapEither :: (a -> Either e b) -> [a] -> ([b], [e])
    mapEither f = swap . partitionEithers . map f

{----- Run -----}

run :: Service -> Options -> IO ()
run service@Service{..} Options{..} = do
  pkg <- loadPackageConfig

  -- resolve targets
  -- TODO: tests
  let resolveTargets' components targets = do
        let (components', unknownTargets) = resolveTargets components targets
        unless (null unknownTargets) $
          Logging.logWarn loggingService $
            "Unknown targets: " <> Text.intercalate ", " unknownTargets
        pure components'
  libs <- resolveTargets' (PackageConfig.packageLibraries pkg) libTargets
  bins <- resolveTargets' (PackageConfig.packageBinaries pkg) binTargets

  -- TODO: getOrCreate lock file

  packageDb <- loadPackageDB distDir

  -- TODO: build dependencies in packageDb
  -- for now, assume dependencies are already built

  -- build libraries
  forM_ (Map.toList libs) $ \(name, libInfo) -> do
    Logging.logInfo loggingService $ "Building library '" <> name <> "'..."
    let libraryId =
          PackageId
            { packageName = name
            , packageVersion = PackageConfig.packageVersion pkg
            }
    let outDir = distDir </> Text.unpack (renderPackageId libraryId)
    buildPlan <- getLibraryBuildPlan service libraryId libInfo
    buildLibrary service packageDb outDir buildPlan
    installLibrary service packageDb outDir buildPlan

  -- build binaries
  Logging.logDebug loggingService "Building binaries..."
  forM_ (Map.toList bins) $ \(_, binInfo) ->
    buildBinary packageDb binInfo

  -- TODO: build tests
  pure ()
  where
    -- TODO: get the directory where the hsproject.toml file is
    projectDir = unsafePerformIO getCurrentDirectory
    -- TODO: get actual GHC version
    ghcVersion = makeVersion [9, 6, 4] -- 9.6.4
    distDir = packageDistDir projectDir ghcVersion

{----- Build library -----}

data LibraryBuildPlan =
  LibraryBuildPlan
    { libraryId :: PackageId
    , libraryConfig :: PackageConfig.LibraryInfo
    , libraryModules :: [(ModuleName, FilePath)]
    }

getLibraryBuildPlan :: Service -> PackageId -> PackageConfig.LibraryInfo -> IO LibraryBuildPlan
getLibraryBuildPlan Service{..} libraryId libraryConfig = do
  -- find modules
  libraryModules <- filter (not . isMainModule . fst) . concat <$> mapM findModulesUnder sourceDirs
  Logging.logDebug loggingService $ "Found modules: " <> showModulesAndPaths libraryModules

  pure LibraryBuildPlan{..}
  where
    PackageConfig.LibraryInfo{sharedInfo} = libraryConfig
    PackageConfig.SharedInfo{sourceDirs} = sharedInfo

    showModulesAndPaths =
      let showModuleAndPath (name, path) = renderModuleName name <> " (" <> Text.pack path <> ")"
       in Text.intercalate ", " . map showModuleAndPath

-- TODO: color logs + parallelize
buildLibrary :: Service -> PackageDB -> FilePath -> LibraryBuildPlan -> IO ()
buildLibrary Service{..} packageDb outDir LibraryBuildPlan{..} = do
  modulesSorted <- sortModules loggingService libraryModules

  -- TODO: find specific GHC, log path to GHC
  -- FIXME: add all transitive deps
  -- FIXME: log command that's running
  -- FIXME: capture logs + stream to file + stream to stdout with DEBUG
  ghcBuild packageDb sharedInfo . concat $
    [ ["-c"] <> map snd modulesSorted
    , ["-odir", outDir]
    , ["-hidir", outDir]
    , ["-this-unit-id", Text.unpack $ renderPackageId libraryId]
    ]

  -- https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/packages.html#building-a-package-from-haskell-source
  let packageFile = outDir </> packageFileName
  let outFiles = map ((outDir </>) . toOutFile . fst) modulesSorted
  callProcess "ar" $ "cqs" : packageFile : outFiles

  Logging.logInfo loggingService $ "Library built: " <> packageName
  where
    PackageId{packageName} = libraryId
    PackageConfig.LibraryInfo{sharedInfo} = libraryConfig

    packageFileName = Text.unpack $ "libHS" <> renderPackageId libraryId <> ".a"
    toOutFile (ModuleName names) = Text.unpack $ Text.intercalate "/" (map unModuleNameId names) <> ".o"

findModulesUnder :: FilePath -> IO [(ModuleName, FilePath)]
findModulesUnder dir = mapMaybe parseModulePath' <$> listFiles defaultOpts dir
  where
    parseModulePath' file = (,dir </> file) <$> parseModulePath file

-- | Sort modules, where latter modules may import earlier modules.
sortModules :: Logging.Service -> [(ModuleName, FilePath)] -> IO [(ModuleName, FilePath)]
sortModules loggingService modulesWithPath = do
  moduleToImports <-
    pooledForConcurrently modulesWithPath $ \(moduleName, path) -> do
      let logProgress =
            Logging.logDebug
              ( Logging.addContext (renderModuleName moduleName)
                  . Logging.addContext "parse-imports"
                  $ loggingService
              )
      logProgress "Running..."
      imports <- parseImports path <$> Text.readFile path
      logProgress "Finished"
      pure ((moduleName, path), imports)

  let (modulesGraph, fromVertex, _) =
        Graph.graphFromEdges
          [ (path, moduleName, imports)
          | ((moduleName, path), imports) <- moduleToImports
          ]
  let getModuleInfo v = let (path, moduleName, _) = fromVertex v in (moduleName, path)
  pure $ map getModuleInfo $ Graph.reverseTopSort modulesGraph

{----- Install library -----}

installLibrary :: Service -> PackageDB -> FilePath -> LibraryBuildPlan -> IO ()
installLibrary Service{..} packageDb installDir LibraryBuildPlan{..} = do
  Logging.logInfo loggingService $ "Installing library " <> packageName <> "..."

  pkgs <- packageDbEntries packageDb
  deps <-
    forM (Map.keys libraryDependencies) $ \dep ->
      case Map.lookup dep pkgs of
        Just depId -> pure depId
        Nothing -> throwIO $ UnknownPackage dep

  registerPackageInDB packageDb $
    InstalledPackageInfo
      { installedPackageId = libraryId
      , installedPackageLocation = installDir
      , installedPackageModules = map fst libraryModules
      , installedPackageDeps = deps
      }

  Logging.logInfo loggingService $ "Library installed: " <> packageName
  where
    PackageId{packageName} = libraryId
    PackageConfig.LibraryInfo{sharedInfo} = libraryConfig
    PackageConfig.SharedInfo{dependencies = libraryDependencies} = sharedInfo

{----- Build binary -----}

buildBinary :: PackageDB -> PackageConfig.BinaryInfo -> IO ()
buildBinary packageDb PackageConfig.BinaryInfo{..} =
  ghcBuild packageDb sharedInfo [mainFile]

{----- GHC -----}

ghcBuild :: PackageDB -> PackageConfig.SharedInfo -> [String] -> IO ()
ghcBuild packageDb PackageConfig.SharedInfo{dependencies} args =
  callProcess "ghc" . concat $
    [ args
    , ["-package-db", packageDbPath packageDb]
    , ["-hide-all-packages"]
    , concatMap (\p -> ["-package", Text.unpack p]) . Map.keys $ dependencies
    ]
