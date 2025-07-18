{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Skelly.Core.Service (
  IsService (..),
  Has,
  getOpts,
  RegistryM,
  loadServiceIO,
  loadService,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift  )
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.State qualified as State
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HList (HList)
import Data.HList qualified as HList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, TypeRep, typeRep)

-- TODO: Move to Skelly.Core.Service
class (Typeable a) => IsService opts a where
  initService :: RegistryM opts a

newtype RegistryM opts a = RegistryM
  { unRegistryM :: ReaderT (HList opts) (StateT Registry IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)

type Registry = Map TypeRep Dynamic

runRegistryM :: HList opts -> RegistryM opts a -> IO a
runRegistryM opts =
  id
    . (`evalStateT` Map.empty)
    . (`runReaderT` opts)
    . unRegistryM

type Has opts allOpts = HList.HOccurs opts (HList allOpts)

getOpts :: Has opts allOpts => RegistryM allOpts opts
getOpts = RegistryM $ Reader.asks HList.hOccurs

-- | The entrypoint; load the given service with the given options.
--
-- All transitive dependencies will be loaded within this call.
loadServiceIO :: (IsService opts a) => HList opts -> IO a
loadServiceIO opts = runRegistryM opts loadService

-- | Load possibly-cached service.
loadService :: forall a opts. (IsService opts a) => RegistryM opts a
loadService = do
  registry <- RegistryM . lift $ State.get
  case Map.lookup repA registry of
    Just dyn ->
      case fromDynamic dyn of
        Just a -> pure a
        Nothing -> error $ "Expected registry to contain " ++ show repA ++ ", got: " ++ show dyn
    Nothing -> do
      a <- initService
      RegistryM . lift $ State.modify (Map.insert repA (toDyn a))
      pure a
  where
    repA = typeRep (Proxy @a)
