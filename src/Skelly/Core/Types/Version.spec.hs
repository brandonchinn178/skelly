{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as Text

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
