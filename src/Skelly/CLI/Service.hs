{-# LANGUAGE RecordWildCards #-}

module Skelly.CLI.Service (
  Service (..),
  Options (..),
  initService,
) where

import Data.Type.Map (Map)
import Skelly.Core.Logging qualified as Logging
import Skelly.Core.PackageIndex qualified as PackageIndex
import Skelly.Core.Utils.Hackage qualified as Hackage
import Skelly.Core.Utils.HTTP qualified as HTTP

-- TODO: Move to Skelly.Core.Service
class IsService opts a where
  initService :: RegistryM opts a

newtype RegistryM opts a = RegistryM
  { unRegistryM :: ReaderT (HList opts) (StateT Registry IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)

type Registry = Map TypeRep Dynamic

runRegistry :: HList opts -> RegistryM opts a -> IO a
runRegistry opts =
  id
    . (`runReaderT` opts)
    . (`execStateT` Map.empty)
    . unRegistryM

type Has opts allOpts = HOccurs opts (HList allOpts)

getOpts :: Has opts allOpts => RegistryM allOpts opts
getOpts = RegistryM $ asks hOccurs

loadService :: (Typeable a, IsService opts a) => RegistryM opts a
loadService = do
  registry <- RegistryM . lift $ get
  case Map.lookup (typeRep @a) registry of
    Just a -> pure a
    Nothing -> do
      a <- initService
      RegistryM . lift $ modify (Map.insert (typeRep @a) a)
      pure a
