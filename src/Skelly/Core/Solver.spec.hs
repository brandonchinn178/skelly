{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Skelly.Core.CompilerEnv (CompilerEnv)
import Skelly.Core.CompilerEnv qualified as CompilerEnv
import Skelly.Core.Error (SkellyError (DependencyResolutionFailure))
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Types.PackageId (
  PackageId (..),
  PackageName,
  parsePackageId,
  renderPackageId,
 )
import Skelly.Core.Types.Version (
  CompiledVersionRange,
  VersionRange,
  compileRange,
  makeVersion,
  parseVersionRange,
  wholeRange,
 )

spec :: Spec
spec = do
  it "gets latest version matching range" $ do
    let
      index =
        [ ("lt-1.1", [])
        , ("lt-2.1", [])
        , ("gt-2.1", [])
        , ("gt-3.1", [])
        , ("pvp-1.4.1", [])
        , ("pvp-1.5.1", [])
        , ("pvp-1.6.1", [])
        , ("any-1", [])
        , ("any-2", [])
        , ("exact-1.2.3", [])
        , ("and-0.1", [])
        , ("and-1.1", [])
        , ("and-2.1", [])
        , ("or-0.1", [])
        , ("or-1.1", [])
        , ("or-2.1", [])
        ]
      input =
        [ ("lt", "< 2")
        , ("gt", "> 3")
        , ("pvp", "^1.5")
        , ("any", "*")
        , ("exact", "1.2.3")
        , ("and", "> 1 && < 2")
        , ("or", "< 1 || > 2")
        ]
      expected =
        [ "and-1.1"
        , "any-2"
        -- TODO: add back
        -- , "exact-1.2.3"
        , "gt-3.1"
        , "lt-1.1"
        , "or-2.1"
        , "pvp-1.5.1"
        ]
    runSolver (mkSimpleService index) input `shouldSatisfy` P.returns (P.eq expected)

  prop "returns topologically sorted deps" $ do
    -- A -> B -> C -> ...
    namesAndRanks <- forAll $ do
      names <- Set.toList <$> Gen.set (Range.linear 1 20) genName
      ranks <- mapM (\_ -> genRank) names
      Gen.shuffle $ zip names ranks
    let names = map fst namesAndRanks
        rankPackage = toRankPackage namesAndRanks

    let index =
          zipWith
            ( \name mDep ->
                ( PackageId name (makeVersion [1, 0])
                , [(dep, "*") | Just dep <- pure mDep]
                )
            )
            names
            (map Just (drop 1 names) ++ [Nothing])
        service =
          defaultTestService
            & withRankPackage rankPackage
            & withIndex index

    let input = [(name, "*") | name <- names]
        expected = reverse names
        getName SolvedPackage{packageId} = packageName packageId

    liftIO (map getName <$> run service defaultEnv (toPackageDeps input))
      `shouldSatisfy` P.returns (P.eq expected)

  -- FIXME
  xfail "not working" . it "throws when resolution fails" $ do
    let index =
          [ ("foo-1.0", [("bar", "1.0")])
          , ("bar-1.0", [])
          , ("bar-2.0", [])
          ]
    runSolver (mkSimpleService index) [("foo", "1.0"), ("bar", "2.0")]
      `shouldSatisfy` P.throws (P.eq DependencyResolutionFailure)

  xfail "fix after resolving TODO" . it "throws when bounds are unsatisfiable" $ do
    runSolver (mkSimpleService [("foo-1.0", [])]) [("foo", "> 100")]
      `shouldSatisfy` P.throws (P.eq DependencyResolutionFailure)

  -- TODO: test helpful message with multiple backtracking failures
  -- TODO: test respects preferredVersionRange

  describe "regression tests" $ do
    forM_ regressionTests $ \RegressionTest{..} ->
      it label $ do
        packages <- run (mkSimpleService index) env (toPackageDeps input)
        map renderSolvedPackageId packages `shouldBe` expected

genName :: Gen PackageName
genName = Gen.text (Range.linear 1 20) $ Gen.frequency [(10, Gen.alphaNum), (1, pure '-')]

genRank :: Gen Int
genRank = Gen.int (Range.linear 0 1000)

toRankPackage :: [(PackageName, Int)] -> (PackageName -> Int)
toRankPackage namesAndRanks = \name -> fromMaybe (-1) $ lookup name namesAndRanks

{----- Helpers -----}

runSolver :: Service -> PackageDepsList -> IO [Text]
runSolver service input = map renderSolvedPackageId <$> run service defaultEnv (toPackageDeps input)

renderSolvedPackageId :: SolvedPackage -> Text
renderSolvedPackageId SolvedPackage{packageId} = renderPackageId packageId

defaultEnv :: Env
defaultEnv =
  Env
    { compilerEnv = defaultCompilerEnv
    , packageFlags = mempty
    }

defaultCompilerEnv :: CompilerEnv
defaultCompilerEnv =
  CompilerEnv.CompilerEnv
    { ghcPath = "/usr/local/bin/ghc"
    , ghcVersion = makeVersion [9, 10, 1]
    , ghcPkgPath = "/usr/local/bin/ghc-pkg"
    , ghcPkgList = Map.empty
    }

toRawPackageDeps :: PackageDepsList -> Map PackageName VersionRange
toRawPackageDeps deps =
  Map.fromList
    [ (name, range)
    | (name, rangeStr) <- deps
    , let range = fromMaybe (error $ "Invalid range: " <> Text.unpack rangeStr) $ parseVersionRange rangeStr
    ]

toPackageDeps :: PackageDepsList -> Map PackageName CompiledVersionRange
toPackageDeps = fmap compile . toRawPackageDeps
  where
    compile r = fromMaybe (error $ "Unsatisfiable range: " <> show r) . compileRange $ r

toPackageId :: Text -> PackageId
toPackageId s =
  case parsePackageId s of
    Just p -> p
    Nothing -> error $ "Invalid package id: " <> Text.unpack s

-- | Dependencies and their version ranges, as a list.
type PackageDepsList = [(Text, Text)]

mkSimpleService :: [(Text, PackageDepsList)] -> Service
mkSimpleService index =
  defaultTestService
    & withIndex (map (first toPackageId) index)

defaultTestService :: Service
defaultTestService =
  Service
    { loggingService = Logging.disabledService
    , withCursor = \f -> f (error "cursor unexpectedly evaluated")
    , getPackageDeps = \_ _ -> error "getPackageDeps not implemented"
    , getPackageVersionInfo = \_ _ -> error "getPackageVersionInfo not implemented"
    , rankPackage = \_ -> 0
    }

withIndex :: [(PackageId, PackageDepsList)] -> Service -> Service
withIndex index service =
  service
    { getPackageDeps = \_ _ PackageId{..} ->
        pure $ indexMap Map.! packageName Map.! packageVersion
    , getPackageVersionInfo = \_ name ->
        pure
          PackageIndex.PackageVersionInfo
            { availableVersions = Map.keys $ indexMap Map.! name
            , preferredVersionRange = wholeRange
            }
    , rankPackage = rankPackageAlpha
    }
  where
    indexMap =
      Map.fromListWith (<>) $
        [ (packageName, Map.singleton packageVersion (toRawPackageDeps deps))
        | (PackageId{..}, deps) <- index
        ]

    -- in tests, iterate over packages in alphabetical order
    rankPackageAlpha name = fromMaybe (-1) $ name `elemIndex` sortedPackages
    sortedPackages = reverse [packageName | (PackageId{..}, _) <- index]

withRankPackage :: (PackageName -> Int) -> Service -> Service
withRankPackage rankPackage service = service{rankPackage = rankPackage}

showVer :: Int -> Text
showVer = Text.pack . show

{----- Regression tests -----}

data RegressionTest = RegressionTest
  { label :: String
  , index :: [(Text, PackageDepsList)]
  , env :: Env
  , input :: PackageDepsList
  , expected :: [Text]
  }

regressionTests :: [RegressionTest]
regressionTests =
  [ RegressionTest
      { label = "base + <lots of branches> + filepath"
      , index =
          concat
            [ [("base-" <> showVer i, []) | i <- [1 .. 10]]
            , [("bytestring-" <> showVer i, []) | i <- [1 .. 10000]]
            , [("containers-" <> showVer i, []) | i <- [1 .. 10000]]
            , [("directory-" <> showVer i, []) | i <- [1 .. 10000]]
            , [("filepath-1", [("base", "< 3")])]
            ]
      , input =
          [ ("base", "*")
          , ("bytestring", "*")
          , ("containers", "*")
          , ("directory", "*")
          , ("filepath", "*")
          ]
      , env = defaultEnv
      , expected =
          [ "base-2"
          , "bytestring-10000"
          , "containers-10000"
          , "directory-10000"
          , "filepath-1"
          ]
      }
  , RegressionTest
      { label = "installed base over newer base"
      , index = [("base-1", []), ("base-2", [])]
      , input =
          [ ("base", "*")
          ]
      , env =
          defaultEnv
            { compilerEnv =
                defaultCompilerEnv
                  { CompilerEnv.ghcPkgList =
                      Map.fromList
                        [ ("base", makeVersion [1])
                        ]
                  }
            }
      , expected =
          [ "base-1"
          ]
      }
  ]
