{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Interval qualified as Interval
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (Version (..))

spec :: Spec
spec = do
  describe "VersionRange" $ do
    describe "renderVersionRange" $ do
      forM_
        [ ("1.0", "1.0")
        , ("> 1 && < 2 || > 3", "(> 1 && < 2) || > 3")
        ] $ \(input, expected) -> do
              it (show input <> " => " <> show expected) $ do
                renderVersionRange (toRange input) `shouldBe` expected

  describe "CompiledVersionRange" $ do
    describe "compileRange" $ do
      it "returns Nothing for impossible conjunctions" $ do
        compileRange (toRange "< 1 && > 2") `shouldBe` Nothing

    describe "inRange" $ do
      forM_
        [ ("1.2.3", "*", True)
        , ("9.9.9", "*", True)
        , ("1.2.0", "< 1.2.3", True)
        , ("1.2.3", "< 1.2.3", False)
        , ("1.2.5", "< 1.2.3", False)
        , ("1.2.0", "<= 1.2.3", True)
        , ("1.2.3", "<= 1.2.3", True)
        , ("1.2.5", "<= 1.2.3", False)
        , ("1.2.0", "1.2.3", False)
        , ("1.2.3", "1.2.3", True)
        , ("1.2.5", "1.2.3", False)
        , ("1.2.0", "!= 1.2.3", True)
        , ("1.2.3", "!= 1.2.3", False)
        , ("1.2.5", "!= 1.2.3", True)
        , ("1.2.0", "> 1.2.3", False)
        , ("1.2.3", "> 1.2.3", False)
        , ("1.2.5", "> 1.2.3", True)
        , ("1.2.0", ">= 1.2.3", False)
        , ("1.2.3", ">= 1.2.3", True)
        , ("1.2.5", ">= 1.2.3", True)
        , ("0.1.0", "^1.2.3", False)
        , ("1.0.0", "^1.2.3", False)
        , ("1.2.0", "^1.2.3", False)
        , ("1.2.9", "^1.2.3", True)
        , ("1.9.0", "^1.2.3", False)
        , ("2.0.0", "^1.2.3", False)
        , ("2.0.0", "^2", True)
        , ("2.1.0", "^2", False)
        , ("1.2.3", "> 1 && < 2", True)
        , ("3.0.0", "> 1 && < 2", False)
        , ("0.0.0", "> 1 && < 2", False)
        , ("1.2.3", "^1.2 || ^1.5", True)
        , ("1.5.3", "^1.2 || ^1.5", True)
        , ("1.3.3", "^1.2 || ^1.5", False)
        ]
        $ \(version, range, expected) -> do
            let op = if expected then "⊆" else "⊈"
            it (show version <> " " <> op <> " " <> show range) $
              inRange (toRangeC range) (toVer version) `shouldBe` expected

    describe "intersectRange" $ do
      it "correctly intersects disjunction with range excluding one" $ do
        Just r <- pure $ intersectRange (toRangeC "^1.2 || ^1.3") (toRangeC "< 1.3")
        inRange r (toVer "1.1") `shouldBe` False
        inRange r (toVer "1.2") `shouldBe` True
        inRange r (toVer "1.3") `shouldBe` False
        inRange r (toVer "1.4") `shouldBe` False

      it "correctly intersects disjunction with range including both" $ do
        Just r <- pure $ intersectRange (toRangeC "^1.2 || ^1.3") (toRangeC "< 2.0")
        inRange r (toVer "1.1") `shouldBe` False
        inRange r (toVer "1.2") `shouldBe` True
        inRange r (toVer "1.3") `shouldBe` True
        inRange r (toVer "1.4") `shouldBe` False

      it "returns Nothing when intersecting disjunction with range excluding both" $
        intersectRange (toRangeC "^1.2 || ^1.3") (toRangeC "< 1.0") `shouldBe` Nothing

      it "correctly intersects NEQ with range" $ do
        Just r <- pure $ intersectRange (toRangeC "!= 1.2") (toRangeC "< 1.4")
        inRange r (toVer "1.1") `shouldBe` True
        inRange r (toVer "1.2") `shouldBe` False
        inRange r (toVer "1.3") `shouldBe` True

      it "returns Nothing when intersecting NEQ with EQ" $ do
        intersectRange (toRangeC "!= 1.2") (toRangeC "1.2") `shouldBe` Nothing

      it "correctly intersects two NEQ" $ do
        Just r <- pure $ intersectRange (toRangeC "!= 1.2") (toRangeC "!= 1.3")
        inRange r (toVer "1.1") `shouldBe` True
        inRange r (toVer "1.2") `shouldBe` False
        inRange r (toVer "1.3") `shouldBe` False
        inRange r (toVer "1.4") `shouldBe` True

    describe "renderCompiledRange" $ do
      forM_
        [ ("1.0", "= 1.0")
        , ("^1.2 || ^1.4", "(≥ 1.2 && < 1.3) || (≥ 1.4 && < 1.5)")
        , ("^1.2 || ^1.3", "≥ 1.2 && < 1.4")
        , ("^1.3 || ^1.2", "≥ 1.2 && < 1.4")
        , ("^1.3 || < 1 || ^1.2", "< 1 || (≥ 1.2 && < 1.4)")
        ] $ \(input, expected) -> do
              it (show input <> " => " <> show expected) $ do
                renderCompiledRange (toRangeC input) `shouldBe` expected

    describe "negateRange" $ do
      it "correctly negates range with disjunction" $ do
        Just r <- pure $ negateRange (toRangeC "1.2 || > 1.3")
        inRange r (toVer "1.1") `shouldBe` True
        inRange r (toVer "1.2") `shouldBe` False
        inRange r (toVer "1.3") `shouldBe` True
        inRange r (toVer "1.4") `shouldBe` False

      prop "inRange (negateRange r) v == not (inRange r v)" $ do
        r <- forAll genRangeC
        v <- forAll genVersion
        Prop.cover 30 "inRange" $ inRange r v
        Prop.cover 30 "not inRange" $ not (inRange r v)
        case negateRange r of
          Just r' -> inRange r' v `shouldBe` not (inRange r v)
          -- range was whole, don't care
          Nothing -> discard

toVer :: Text -> Version
toVer s =
  case parseVersion s of
    Just v -> v
    Nothing -> error $ "Invalid version: " <> Text.unpack s

toRange :: Text -> VersionRange
toRange s =
  case parseVersionRange s of
    Just r -> r
    Nothing -> error $ "Invalid range: " <> Text.unpack s

toRangeC :: Text -> CompiledVersionRange
toRangeC s =
  case compileRange (toRange s) of
    Just r -> r
    Nothing -> error $ "Invalid range: " <> Text.unpack s

genRangeC :: Gen.Gen CompiledVersionRange
genRangeC =
  fmap unsafeCompiledVersionRange $ do
    range <-
      Gen.frequency
        [ (10, pure $ Range.singleton 1)
        , (1, pure $ Range.exponential 1 10)
        ]
    Gen.nonEmpty range genInterval
  where
    genInterval = do
      bound <- genBound
      Gen.frequency
        [ (20, pure $ Interval.interval negInf bound)
        , (20, Interval.interval bound <$> genBoundGT bound)
        , (20, pure $ Interval.interval bound posInf)
        -- whole range
        , (1, pure $ Interval.interval negInf posInf)
        -- singleton
        , (1, Interval.singleton <$> genVersion)
        ]

    negInf = (Interval.NegInf, Interval.Open)
    posInf = (Interval.PosInf, Interval.Open)

    genBoundary = Gen.element [Interval.Open, Interval.Closed]

    genBound = do
      v <- genVersion
      b <- genBoundary
      pure (Interval.Finite v, b)

    genBoundGT (lo, _) = do
      Interval.Finite (Version{versionBranch = [major, minor, patch]}) <- pure lo
      hi <-
        Gen.choice -- choose whether major bump, minor bump, or patch bump
          [ do
              major' <- (+ 1) <$> Gen.int (Range.exponential major 10)
              minor' <- Gen.int (Range.exponential 0 100)
              patch' <- Gen.int (Range.exponential 0 100)
              pure $ makeVersion [major', minor', patch']
          , do
              minor' <- (+ 1) <$> Gen.int (Range.exponential minor 100)
              patch' <- Gen.int (Range.exponential 0 100)
              pure $ makeVersion [major, minor', patch']
          , do
              patch' <- (+ 1) <$> Gen.int (Range.exponential patch 100)
              pure $ makeVersion [major, minor, patch']
          ]
      b <- genBoundary
      pure (Interval.Finite hi, b)

genVersion :: Gen.Gen Version
genVersion = do
  major <- Gen.int (Range.exponential 0 10)
  minor <- Gen.int (Range.exponential 0 100)
  patch <- Gen.int (Range.exponential 0 100)
  pure $ makeVersion [major, minor, patch]
