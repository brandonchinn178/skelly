{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (elemIndex)
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
import Skelly.Core.Types.Version (makeVersion, parseVersionRange, wholeRange)

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
    runSolver (mkService index) input `shouldSatisfy` P.returns (P.eq expected)

  prop "returns topologically sorted deps" $ do
    -- A -> B -> C -> ...
    namesAndRanks <- forAll $ do
      names <- Set.toList <$> Gen.set (Range.linear 1 20) genName
      ranks <- mapM (\_ -> genRank) names
      Gen.shuffle $ zip names ranks
    let names = map fst namesAndRanks
        rankPackage = toRankPackage namesAndRanks
        index =
          zipWith
            ( \name mDep ->
                ( PackageId name (makeVersion [1, 0])
                , [(dep, "*") | Just dep <- pure mDep]
                )
            )
            names
            (map Just (drop 1 names) ++ [Nothing])
        input = [(name, "*") | name <- names]
        expected = reverse names
    liftIO (run (mkService' index rankPackage) defaultEnv (toPackageDeps input))
      `shouldSatisfy` P.returns (map packageName P.>>> P.eq expected)

  it "throws when resolution fails" $ do
    runSolver (mkService [("foo-2.0", [])]) [("foo", "^1.0")]
      `shouldSatisfy` P.throws (P.eq DependencyResolutionFailure)

  -- TODO: test helpful message with multiple backtracking failures
  -- TODO: test respects preferredVersionRange

  describe "regression tests" $ do
    forM_ regressionTests $ \RegressionTest{..} ->
      it label $ do
        packages <- run (mkService index) env (toPackageDeps input)
        map renderPackageId packages `shouldBe` expected

genName :: Gen PackageName
genName = Gen.text (Range.linear 1 20) $ Gen.frequency [(10, Gen.alphaNum), (1, pure '-')]

genRank :: Gen Int
genRank = Gen.int (Range.linear 0 1000)

toRankPackage :: [(PackageName, Int)] -> (PackageName -> Int)
toRankPackage namesAndRanks = \name -> fromMaybe (-1) $ lookup name namesAndRanks

{----- Helpers -----}

runSolver :: Service -> PackageDepsList -> IO [Text]
runSolver service input = map renderPackageId <$> run service defaultEnv (toPackageDeps input)

defaultEnv :: CompilerEnv
defaultEnv =
  CompilerEnv.CompilerEnv
    { ghcPath = "/usr/local/bin/ghc"
    , ghcVersion = makeVersion [9, 10, 1]
    , ghcPkgPath = "/usr/local/bin/ghc-pkg"
    , ghcPkgList = Map.empty
    }

toPackageDeps :: PackageDepsList -> PackageDeps
toPackageDeps deps =
  Map.fromList
    [ (name, range)
    | (name, rangeStr) <- deps
    , let range = fromMaybe (error $ "Invalid range: " <> Text.unpack rangeStr) $ parseVersionRange rangeStr
    ]

toPackageId :: Text -> PackageId
toPackageId s =
  case parsePackageId s of
    Just p -> p
    Nothing -> error $ "Invalid package id: " <> Text.unpack s

-- | Dependencies and their version ranges, as a list.
type PackageDepsList = [(Text, Text)]

mkService :: [(Text, PackageDepsList)] -> Service
mkService index = mkService' index' rankPackageAlpha
  where
    index' = map (first toPackageId) index

    -- in tests, iterate over packages in alphabetical order
    rankPackageAlpha name = fromMaybe (-1) $ name `elemIndex` sortedPackages
    sortedPackages = reverse [packageName | (PackageId{..}, _) <- index']

mkService' :: [(PackageId, PackageDepsList)] -> (PackageName -> Int) -> Service
mkService' index rankPackage =
  Service
    { loggingService = Logging.disabledService
    , withCursor = \f -> f undefined
    , getPackageDeps = \_ PackageId{..} -> pure $ indexMap Map.! packageName Map.! packageVersion
    , getPackageVersionInfo = \_ name ->
        pure
          PackageIndex.PackageVersionInfo
            { availableVersions = Map.keys $ indexMap Map.! name
            , preferredVersionRange = wholeRange
            }
    , rankPackage
    }
  where
    indexMap =
      Map.fromListWith (<>) $
        [ (packageName, Map.singleton packageVersion (toPackageDeps deps))
        | (PackageId{..}, deps) <- index
        ]

showVer :: Int -> Text
showVer = Text.pack . show

{----- Regression tests -----}

data RegressionTest = RegressionTest
  { label :: String
  , index :: [(Text, PackageDepsList)]
  , env :: CompilerEnv
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
            { CompilerEnv.ghcPkgList =
                Map.fromList
                  [ ("base", makeVersion [1])
                  ]
            }
      , expected =
          [ "base-1"
          ]
      }
  ]
