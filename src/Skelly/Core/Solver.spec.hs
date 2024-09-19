{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Skelly.Core.Error (SkellyError (DependencyResolutionFailure))
import Skelly.Core.Utils.PackageId (PackageId (..), renderPackageId)
import Skelly.Core.Utils.Version (makeVersion, parseVersion, parseVersionRange)

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
    names <- forAll $ Gen.shuffle . Set.toList =<< Gen.set (Range.linear 1 20) genName
    let index =
          zipWith
            ( \name mDep ->
                ( PackageId name (makeVersion [1, 0])
                , [(dep, "*") | Just dep <- pure mDep]
                )
            )
            names
            (map Just (tail names) ++ [Nothing])
        input = [(name, "*") | name <- names]
        expected = reverse names
    liftIO (run (mkService' index) (toPackageDeps input))
      `shouldSatisfy` P.returns (map packageName P.>>> P.eq expected)

  prop "efficiently backtracks" $ do
    -- project => A, B
    -- A (2.1, 2.2, ..., 1.0, 2.10, 2.11, ...) => []
    -- B (1.0) => A ^1.0
    nameA <- forAll genName
    nameB <- forAll $ Gen.filterT (/= nameA) genName
    numA_2_before <- forAll $ Gen.int (Range.linear 1 100)
    numA_2_after <- forAll $ Gen.int (Range.linear 1 100)
    let index =
          concat
            [ [(PackageId nameA (makeVersion [2, i]), []) | i <- [1 .. numA_2_before]]
            , [(PackageId nameA (makeVersion [1, 0]), [])]
            , [(PackageId nameA (makeVersion [2, numA_2_before + i]), []) | i <- [1 .. numA_2_after]]
            , [(PackageId nameB (makeVersion [1, 0]), [(nameA, "^1.0")])]
            ]
        input = toPackageDeps [(nameA, "*"), (nameB, "*")]
        expected =
          [ PackageId nameA (makeVersion [1, 0])
          , PackageId nameB (makeVersion [1, 0])
          ]
    liftIO (run (mkService' index) input) `shouldSatisfy` P.returns (P.eq expected)

  it "throws when resolution fails" $ do
    runSolver (mkService [("foo-2.0", [])]) [("foo", "^1.0")]
      `shouldSatisfy` P.throws (P.eq DependencyResolutionFailure)

genName :: Gen Text
genName = Gen.text (Range.linear 1 20) $ Gen.frequency [(10, Gen.alphaNum), (1, pure '-')]

{----- Helpers -----}

runSolver :: Service -> PackageDepsList -> IO [Text]
runSolver service input = map renderPackageId <$> run service (toPackageDeps input)

toPackageDeps :: PackageDepsList -> PackageDeps
toPackageDeps deps =
  Map.fromList
    [ (name, range)
    | (name, rangeStr) <- deps
    , let range = fromMaybe (error $ "Invalid range: " <> Text.unpack rangeStr) $ parseVersionRange rangeStr
    ]

toPackageId :: Text -> PackageId
toPackageId s =
  case Text.breakOnEnd "-" s of
    (pre, versionStr)
      | let name = Text.dropEnd 1 pre
      , Just version <- parseVersion versionStr ->
          PackageId name version
    _ -> error $ "Invalid package id: " <> Text.unpack s

-- | Dependencies and their version ranges, as a list.
type PackageDepsList = [(Text, Text)]

mkService :: [(Text, PackageDepsList)] -> Service
mkService = mkService' . map (first toPackageId)

mkService' :: [(PackageId, PackageDepsList)] -> Service
mkService' index =
  Service
    { withCursor = \f -> f undefined
    , getPackageDeps = \_ PackageId{..} -> pure $ indexMap Map.! packageName Map.! packageVersion
    , getPackageVersions = \_ name -> pure . Map.keys $ indexMap Map.! name
    }
  where
    indexMap =
      Map.fromListWith (<>) $
        [ (packageName, Map.singleton packageVersion (toPackageDeps deps))
        | (PackageId{..}, deps) <- index
        ]
