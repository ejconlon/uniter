module Uniter.FreeEnv
  ( FreeEnv
  , FreeEnvLens
  , emptyFreeEnv
  , toListFreeEnv
  , insertFreeEnv
  , insertFreeEnvLM
  , insertFreeEnvM
  , insertAllFreeEnv
  , insertAllFreeEnvLM
  , insertAllFreeEnvM
  , lookupFreeEnv
  , lookupFreeEnvLM
  , lookupFreeEnvM
  , FreeEnvMissingErr (..)
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

insertAllFreeEnv :: (Foldable f, Ord a) => f (a, BoundId) -> FreeEnv a -> FreeEnv a
insertAllFreeEnv ps m = foldr (\(a, b) n -> insertFreeEnv a b n) m ps

insertAllFreeEnvLM :: (MonadReader r m, Foldable f, Ord a) => FreeEnvLens a r -> f (a, BoundId) -> m c -> m c
insertAllFreeEnvLM l ps = local (over l (insertAllFreeEnv ps))

insertAllFreeEnvM :: (MonadReader (FreeEnv a) m, Foldable f, Ord a) => f (a, BoundId) -> m c -> m c
insertAllFreeEnvM = insertAllFreeEnvLM id

lookupFreeEnv :: Ord a => a -> FreeEnv a -> Maybe BoundId
lookupFreeEnv = Map.lookup

lookupFreeEnvLM :: (MonadReader r m, Ord a) => FreeEnvLens a r -> a -> m (Maybe BoundId)
lookupFreeEnvLM l a = fmap (lookupFreeEnv a) (view l)

lookupFreeEnvM :: (MonadReader (FreeEnv a) m, Ord a) => a -> m (Maybe BoundId)
lookupFreeEnvM = lookupFreeEnvLM id

newtype FreeEnvMissingErr a = FreeEnvMissingErr { unFreeEnvMissingErr :: a }
  deriving newtype (Eq, Ord, Typeable)
  deriving stock (Show)

instance (Show a, Typeable a) => Exception (FreeEnvMissingErr a)
