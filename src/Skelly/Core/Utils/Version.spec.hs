{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "CompiledVersionRange" $ do
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
        ]
        $ \(versionStr, rangeStr, expected) -> do
            let op = if expected then "⊆" else "⊈"
            it (show versionStr <> " " <> op <> " " <> show rangeStr) $ do
              Just version <- pure $ parseVersion versionStr
              Just range <- pure $ parseVersionRange rangeStr
              inRange (compileRange range) version `shouldBe` expected
