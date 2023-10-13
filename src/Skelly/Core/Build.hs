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

import Control.Monad (forM_, unless)
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
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageConfig (PackageConfig)
import Skelly.Core.PackageConfig qualified as PackageConfig
import Skelly.Core.Parse (parseImports)
import Skelly.Core.Utils.Default (defaultOpts)
import Skelly.Core.Utils.Modules (
  ModuleName,
  isMainModule,
  parseModulePath,
  renderModuleName,
 )
import Skelly.Core.Utils.Path (listFiles)
import System.FilePath ((</>))
import UnliftIO.Async (pooledForConcurrently)

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

  -- TODO: build dependencies
  -- for now, assume dependencies are registered here
  let packageDb = "~/.cabal/store/ghc-9.6.2/package.db"

  -- build libraries
  forM_ (Map.toList libs) $ \(name, libInfo) ->
    buildLibrary service packageDb name libInfo

  -- build binaries
  Logging.logDebug loggingService "Building binaries..."
  -- FIXME: ghc <mainFile> -package <lib>
  error $ "build bins: " <> show bins

  -- TODO: build tests

-- TODO: color logs + parallelize
buildLibrary :: Service -> FilePath -> Text -> PackageConfig.LibraryInfo -> IO ()
buildLibrary Service{..} packageDb libName PackageConfig.LibraryInfo{..} = do
  Logging.logInfo loggingService $ "Building library '" <> libName <> "'..."

  modules <- filter (not . isMainModule . fst) . concat <$> mapM findModulesUnder sourceDirs
  Logging.logDebug loggingService $ "Found modules: " <> showModulesAndPaths modules

  modulesSorted <- sortModules loggingService modules

  error . unlines $
    [ "FIXME: ghc -c <modules in order> -package-db $packageDb -package $deps -odir <odir> -hidir <hidir>"
    , show packageDb
    , show (map renderModuleName modulesSorted)
    ]
    :: IO ()
  error "FIXME: build .a package file manually" :: IO ()
  where
    PackageConfig.SharedInfo{sourceDirs} = sharedInfo

    showModulesAndPaths =
      let showModuleAndPath (name, path) = renderModuleName name <> " (" <> Text.pack path <> ")"
       in Text.intercalate ", " . map showModuleAndPath

findModulesUnder :: FilePath -> IO [(ModuleName, FilePath)]
findModulesUnder dir = mapMaybe parseModulePath' <$> listFiles defaultOpts dir
  where
    parseModulePath' file = (,dir </> file) <$> parseModulePath file

-- | Sort modules, where latter modules may import earlier modules.
sortModules :: Logging.Service -> [(ModuleName, FilePath)] -> IO [ModuleName]
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
      pure (moduleName, imports)

  let (modulesGraph, fromVertex, _) =
        Graph.graphFromEdges
          [ ((), moduleName, imports)
          | (moduleName, imports) <- moduleToImports
          ]
  let fromVertex' v = let (_, key, _) = fromVertex v in key
  pure $ map fromVertex' $ Graph.reverseTopSort modulesGraph

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
