{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Skelly.Core.Build (
  -- * Service
  Service (..),

  -- * Methods
  run,
  Options (..),
  allOptionTargets,
  Targets (..),
) where

import Control.Monad (forM_, unless)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Tuple (swap)
import Skelly.Core.CompilerEnv (CompilerEnv)
import Skelly.Core.CompilerEnv qualified as CompilerEnv
import Skelly.Core.Logging (logDebug, logWarn)
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageConfig (PackageConfig)
import Skelly.Core.PackageConfig qualified as PackageConfig
import Skelly.Core.PackageIndex qualified as PackageIndex
-- import Skelly.Core.Parse (parseImports)
import Skelly.Core.Paths (packageDistDir, skellyCacheDir)
import Skelly.Core.Service (IsService (..), loadService)
-- import Skelly.Core.Solver qualified as Solver
import Skelly.Core.Types.PackageId (PackageId (PackageId), renderPackageId)
import Skelly.Core.Types.PackageId qualified as PackageId
import Skelly.Core.Types.Version (
  Version,
  VersionRange (VersionRangeAnd),
  makeVersion,
 )
import Skelly.Core.Utils.Default (defaultOpts)
import Skelly.Core.Utils.Modules (
  ModuleName,
  isMainModule,
  parseModulePath,
  renderModuleName,
 )
import Skelly.Core.Utils.Path (listFiles)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Exit (ExitCode (..), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import System.Process qualified as Process
-- import UnliftIO.Async (pooledForConcurrently)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist)
import UnliftIO.Temporary (withSystemTempDirectory)

data Service = Service
  { loggingService :: Logging.Service
  , pkgIndexService :: PackageIndex.Service
  -- , solveDeps :: CompilerEnv -> Solver.PackageDeps -> IO [PackageId]
  , loadPackageConfig :: IO PackageConfig
  , loadCompilerEnv :: Version -> IO CompilerEnv
  }

instance
  ( IsService opts Logging.Service
  , IsService opts PackageIndex.Service
  ) => IsService opts Service where
  initService = do
    loggingService <- loadService
    pkgIndexService <- loadService
    -- solverService <- loadService
    pure
      Service
        { loggingService
        -- , solveDeps = Solver.run solverService
        , pkgIndexService
        , loadPackageConfig = PackageConfig.loadPackageConfig loggingService
        , loadCompilerEnv = CompilerEnv.loadCompilerEnv
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
          logWarn loggingService $
            "Unknown targets: " <> Text.intercalate ", " unknownTargets
        pure components'
  libs <- resolveTargets' (PackageConfig.packageLibraries pkg) libTargets
  bins <- resolveTargets' (PackageConfig.packageBinaries pkg) binTargets

  -- load build environment
  let ghcVersion = makeVersion [9, 10, 1] -- TODO: decide version from hspackage.toml
  env <- loadCompilerEnv ghcVersion
  let distDir = packageDistDir projectDir (CompilerEnv.ghcVersion env)

  -- TODO: check for Hackage updates
  -- TODO: getOrCreate lock file
  let
    libDeps =
      [ dependencies
      | PackageConfig.LibraryInfo{sharedInfo} <- Map.elems libs
      , let PackageConfig.SharedInfo{dependencies} = sharedInfo
      ]
    binDeps =
      [ dependencies
      | PackageConfig.BinaryInfo{sharedInfo} <- Map.elems bins
      , let PackageConfig.SharedInfo{dependencies} = sharedInfo
      ]
    allDeps = Map.unionsWith VersionRangeAnd (libDeps <> binDeps)
  logDebug loggingService "Resolving dependencies..."
  -- allTransitiveDeps <- solveDeps env allDeps
  let allTransitiveDeps = undefined env allDeps :: [PackageId]
  logDebug loggingService $ "allTransitiveDeps = " <> Text.pack (show allTransitiveDeps)

  -- download deps
  depInfos <-
    PackageIndex.withPackageIndex pkgIndexService $ \index ->
      PackageIndex.withIndexCursor index $ \cursor ->
        mapM (downloadDep index cursor) allTransitiveDeps

  -- get library build plan
  libPlans <-
    flip mapM (Map.toList libs) $ \(name, libInfo) -> do
      let libraryId =
            PackageId
              { packageName = name
              , packageVersion = PackageConfig.packageVersion pkg
              }
      getLibraryBuildPlan service libraryId libInfo

  -- rewrite binary files as module files
  let binDir = distDir </> "gen"
  let binFiles =
        [ ( binName -- name of binary
          , mainFile -- original file
          , binDir </> Text.unpack ("Main_" <> binName <> ".hs") -- move file here
          , binDir </> Text.unpack (binName <> ".hs") -- new file
          )
        | (binName, PackageConfig.BinaryInfo{mainFile}) <- Map.toList bins
        ]
  createDirectoryIfMissing True binDir
  forM_ binFiles $ \(binName, binSrc, binModule, binNew) -> do
    binContents <- Text.readFile binSrc
    let modName = "Main_" <> binName
    -- TODO: allow omitted Main module header
    Text.writeFile binModule $ Text.replace "module Main" ("module " <> modName) binContents
    Text.writeFile binNew . Text.unlines $
      [ "{-# OPTIONS_GHC -w #-}"
      , "import qualified " <> modName
      , "main = " <> modName <> ".main"
      ]

  -- batch build
  let libDirs =
        [ dir
        | lib <- Map.elems $ PackageConfig.packageLibraries pkg
        , let PackageConfig.LibraryInfo{sharedInfo} = lib
        , let PackageConfig.SharedInfo{sourceDirs} = sharedInfo
        , dir <- sourceDirs
        ]
  ghcBuild loggingService env projectDir . concat $
    [ ["-odir", distDir </> "out"]
    , ["-hidir", distDir </> "out"]
    , ["-i" <> dir | DepInfo{..} <- depInfos, dir <- depSrcDirs]
    , ["-i" <> dir | dir <- libDirs]
    , [fp | LibraryBuildPlan{libraryModules} <- libPlans, (_, fp) <- libraryModules]
    , [binModule | (_, _, binModule, _) <- binFiles]
    ]

  -- build binaries
  createDirectoryIfMissing True $ distDir </> "bin"
  forM_ binFiles $ \(binName, _, binModule, binNew) ->
    ghcBuild loggingService env projectDir . concat $
      [ ["-odir", distDir </> "out"]
      , ["-hidir", distDir </> "out"]
      , ["-i" <> dir | DepInfo{..} <- depInfos, dir <- depSrcDirs]
      , ["-i" <> dir | dir <- libDirs]
      , ["-i" <> takeDirectory binModule]
      , [binNew]
      , ["-o", distDir </> "bin" </> Text.unpack binName]
      ]
  where
    -- TODO: get actual directory where the hsproject.toml file is
    projectDir = unsafePerformIO getCurrentDirectory

data DepInfo = DepInfo
  { depSrcDirs :: [FilePath]
  }

downloadDep :: PackageIndex.PackageIndex -> PackageIndex.PackageIndexCursor -> PackageId -> IO DepInfo
downloadDep index cursor pkgId = do
  pkgInfo <- PackageIndex.getPackageInfo cursor pkgId
  let srcDirs = map (dest </>) $ PackageIndex.packageSrcDirs pkgInfo

  exists <- doesDirectoryExist dest
  unless exists $ do
    -- TODO: don't download built-in packages like base, text, etc. use already precompiled versions
    -- download package files
    PackageIndex.downloadPackage index pkgId (takeDirectory dest) -- TODO: pass in exact dest instead of the parent

    -- apply Cabal options to files
    let header = "{-# LANGUAGE " <> (Text.intercalate "," . PackageIndex.packageDefaultExtensions) pkgInfo <> " #-}\n"
    forM_ srcDirs $ \srcDir -> do
      modules <- findModulesUnder srcDir
      forM_ modules $ \(_, fp) ->
        Text.writeFile fp . (header <>) =<< Text.readFile fp

    -- TODO: write autogen cabal files

  pure
    DepInfo
      { depSrcDirs = srcDirs
      }
  where
    dest = skellyCacheDir </> "packages" </> (Text.unpack . renderPackageId) pkgId

{----- Build library -----}

-- TODO: cleanup
data LibraryBuildPlan =
  LibraryBuildPlan
    { libraryId :: PackageId
    , libraryConfig :: PackageConfig.LibraryInfo
    , libraryModules :: [(ModuleName, FilePath)]
    , librarySrcDirs :: [FilePath]
    }

getLibraryBuildPlan :: Service -> PackageId -> PackageConfig.LibraryInfo -> IO LibraryBuildPlan
getLibraryBuildPlan Service{..} libraryId libraryConfig = do
  -- find modules
  libraryModules <- filter (not . isMainModule . fst) . concat <$> mapM findModulesUnder sourceDirs
  logDebug loggingService $ "Found modules: " <> showModulesAndPaths libraryModules

  let librarySrcDirs = sourceDirs

  pure LibraryBuildPlan{..}
  where
    PackageConfig.LibraryInfo{sharedInfo} = libraryConfig
    PackageConfig.SharedInfo{sourceDirs} = sharedInfo

    showModulesAndPaths =
      let showModuleAndPath (name, path) = renderModuleName name <> " (" <> Text.pack path <> ")"
       in Text.intercalate ", " . map showModuleAndPath

findModulesUnder :: FilePath -> IO [(ModuleName, FilePath)]
findModulesUnder dir = mapMaybe parseModulePath' <$> listFiles defaultOpts dir
  where
    parseModulePath' file = (,dir </> file) <$> parseModulePath file

-- -- | Sort modules, where latter modules may import earlier modules.
-- sortModules :: Logging.Service -> [(ModuleName, FilePath)] -> IO [(ModuleName, FilePath)]
-- sortModules loggingService modulesWithPath = do
--   moduleToImports <-
--     pooledForConcurrently modulesWithPath $ \(moduleName, path) -> do
--       let logProgress =
--             logDebug
--               ( Logging.addContext (renderModuleName moduleName)
--                   . Logging.addContext "parse-imports"
--                   $ loggingService
--               )
--       logProgress "Running..."
--       imports <- parseImports path <$> Text.readFile path
--       logProgress "Finished"
--       pure ((moduleName, path), imports)
--
--   let (modulesGraph, fromVertex, _) =
--         Graph.graphFromEdges
--           [ (path, moduleName, imports)
--           | ((moduleName, path), imports) <- moduleToImports
--           ]
--   let getModuleInfo v = let (path, moduleName, _) = fromVertex v in (moduleName, path)
--   pure $ map getModuleInfo $ Graph.reverseTopSort modulesGraph

{----- GHC -----}

-- TODO: log what modules are being built
ghcBuild :: Logging.Service -> CompilerEnv -> FilePath -> [String] -> IO ()
ghcBuild loggingService env cwd args' = withSystemTempDirectory "skelly-ghc" $ \tmpdir -> do
  logDebug loggingService $ "Running ghc: " <> (Text.pack . show) args

  let argFile = tmpdir </> "ghc-args"
  writeFile argFile (unlines args)

  let ghcProc =
        (Process.proc (CompilerEnv.ghcPath env) ["@" <> argFile])
          { Process.delegate_ctlc = True
          , Process.cwd = Just cwd
          }
  Process.withCreateProcess ghcProc $ \_ _ _ h ->
    Process.waitForProcess h >>= \case
      ExitSuccess -> pure ()
      code@(ExitFailure _) -> exitWith code
  where
    args =
      -- TODO: allow specifying ghc options on command line
      -- TODO: -j
      -- TODO: -O2 for release mode
      -- TODO: -Wall
      -- TODO: -Werror for only local
      concat
        [ case Logging.getLogLevel loggingService of
            Logging.LevelDebug -> ["-v1"]
            Logging.LevelInfo -> ["-v1"]
            Logging.LevelWarn -> ["-v1"]
            Logging.LevelError -> ["-v0"]
        , args'
        ]
