module Uniter.FreeEnv
  ( FreeEnv
  , FreeEnvLens
  , emptyFreeEnv
  , toListFreeEnv
  , insertFreeEnv
  , insertFreeEnvLM
  , insertFreeEnvM
  , lookupFreeEnv
  , lookupFreeEnvLM
  , lookupFreeEnvM
  , FreeEnvMissingError (..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Lens.Micro (Lens', over)
import Lens.Micro.Mtl (view)
import Uniter.Core (BoundId)

type FreeEnv a = Map a BoundId

type FreeEnvLens a s = Lens' s (FreeEnv a)

emptyFreeEnv :: FreeEnv a
emptyFreeEnv = Map.empty

toListFreeEnv :: FreeEnv a -> [(a, BoundId)]
toListFreeEnv = Map.toList

insertFreeEnv :: Ord a => a -> BoundId -> FreeEnv a -> FreeEnv a
insertFreeEnv = Map.insert

insertFreeEnvLM :: (MonadReader r m, Ord a) => FreeEnvLens a r -> a -> BoundId -> m c -> m c
insertFreeEnvLM l a b = local (over l (insertFreeEnv a b))

insertFreeEnvM :: (MonadReader (FreeEnv a) m, Ord a) => a -> BoundId -> m c -> m c
insertFreeEnvM = insertFreeEnvLM id

lookupFreeEnv :: Ord a => a -> FreeEnv a -> Maybe BoundId
lookupFreeEnv = Map.lookup

lookupFreeEnvLM :: (MonadReader r m, Ord a) => FreeEnvLens a r -> a -> m (Maybe BoundId)
lookupFreeEnvLM l a = fmap (lookupFreeEnv a) (view l)

lookupFreeEnvM :: (MonadReader (FreeEnv a) m, Ord a) => a -> m (Maybe BoundId)
lookupFreeEnvM = lookupFreeEnvLM id

newtype FreeEnvMissingError a = FreeEnvMissingError { unFreeEnvMissingError :: a }
  deriving newtype (Eq, Typeable)
  deriving stock (Show)

instance (Show a, Typeable a) => Exception (FreeEnvMissingError a)
