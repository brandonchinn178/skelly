{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Skelly.Core.Utils.KDL (
  remainingUniqueNodesWith,

  -- * KDLQuery
  KDLQuery,
  (>),
  element,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import KDL qualified
import Prelude hiding ((>))

-- https://github.com/brandonchinn178/kdl-hs/issues/23
remainingUniqueNodesWith :: (Typeable a) => KDL.NodeDecoder a -> KDL.NodeListDecoder (Map Text a)
remainingUniqueNodesWith d = KDL.remainingNodesWith d >>= Map.traverseWithKey toUnique
 where
  toUnique name = \case
    [x] -> pure x
    _ -> KDL.fail $ "Nodes must be unique, found duplicate nodes named: " <> name

-- https://github.com/brandonchinn178/kdl-hs/issues/26
instance HasField "modifyNodesMatching" KDL.NodeList (KDLQuery -> (KDL.Node -> KDL.Node) -> KDL.NodeList) where
  getField = error "modifyNodesMatching not implemented yet"

data KDLQuery
  = KQL_Direct Text KDLQuery
  | KQL_Element Text

instance IsString KDLQuery where
  fromString = element . fromString

element :: Text -> KDLQuery
element = KQL_Element

(>) :: Text -> KDLQuery -> KDLQuery
(>) = KQL_Direct
infixr 4 >
