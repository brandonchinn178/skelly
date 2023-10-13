{-# LANGUAGE OverloadedStrings #-}

module Skelly.Core.Parse (
  parseImports,
) where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void (Void)
import Skelly.Core.Utils.Modules (ModuleName, parseModuleName)
import Text.Megaparsec
import Text.Megaparsec.Char

{----- parseImports -----}

type Parser = Parsec Void Text

parseImports :: FilePath -> Text -> [ModuleName]
parseImports fp input =
  case runParser importsParser fp input of
    Right imports -> imports
    Left e -> error $ "parseImports unexpectedly failed: " <> errorBundlePretty e

importsParser :: Parser [ModuleName]
importsParser = catMaybes <$> many (notFollowedBy eof *> parseNextImport)
  where
    -- precondition: parser at beginning of line
    -- postcondition: parser at beginning of line
    parseNextImport =
      optional (try $ space *> string "import" *> space1 *> importModuleNameParser)
        <* takeWhileP Nothing (/= '\n') -- ignore everything up to the newline character
        <* takeP Nothing 1 -- skip the newline character

    importModuleNameParser = do
      moduleName <-
        optional (string "qualified" *> space1)
          *> space
          *> takeWhileP Nothing (not . isSpace)
      maybe (failure Nothing mempty) pure $ parseModuleName moduleName
