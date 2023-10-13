{-# LANGUAGE OverloadedStrings #-}

module Skelly.Core.Utils.Modules (
  -- * ModuleName
  ModuleName (..),
  parseModulePath,
  parseModuleName,
  renderModuleName,
  isMainModule,

  -- * ModuleNameId
  ModuleNameId (..),
  parseModuleNameId,
) where

import Control.Monad (guard)
import Data.Char (isDigit, isLower, isUpper)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (dropExtensions, splitDirectories, takeExtensions)

newtype ModuleName = ModuleName {unModuleName :: [ModuleNameId]}
  deriving (Show, Eq, Ord)

parseModulePath :: FilePath -> Maybe ModuleName
parseModulePath file = do
  guard $ takeExtensions file == ".hs"
  let moduleComponents = splitDirectories $ dropExtensions file
  ModuleName <$> mapM (parseModuleNameId . Text.pack) moduleComponents

parseModuleName :: Text -> Maybe ModuleName
parseModuleName = fmap ModuleName . mapM parseModuleNameId . Text.splitOn "."

renderModuleName :: ModuleName -> Text
renderModuleName = Text.intercalate "." . map unModuleNameId . unModuleName

isMainModule :: ModuleName -> Bool
isMainModule (ModuleName nameIds) = nameIds == [ModuleNameId "Main"]

-- | A 'modid' as defined in: https://www.haskell.org/onlinereport/lexemes.html#sect2.4
newtype ModuleNameId = ModuleNameId {unModuleNameId :: Text}
  deriving (Show, Eq, Ord)

parseModuleNameId :: Text -> Maybe ModuleNameId
parseModuleNameId nameId = do
  (c, cs) <- Text.uncons nameId
  guard $ isUpper c
  guard $ Text.all (\x -> any ($ x) [isLower, isUpper, isDigit, (== '\'')]) cs
  pure $ ModuleNameId nameId
