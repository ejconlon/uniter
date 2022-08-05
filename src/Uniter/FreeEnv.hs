module Uniter.FreeEnv
  ( FreeEnv
  , FreeEnvLens
  , empty
  , toList
  , fromList
  , insert
  , insertLM
  , insertM
  , insertAll
  , insertAllLM
  , insertAllM
  , lookup
  , lookupLM
  , lookupM
  , FreeEnvMissingErr (..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Lens.Micro (Lens', over)
import Lens.Micro.Mtl (view)
import Prelude hiding (lookup)
import Uniter.Core (BoundId)

type FreeEnv a = Map a BoundId

type FreeEnvLens a s = Lens' s (FreeEnv a)

empty :: FreeEnv a
empty = Map.empty

toList :: FreeEnv a -> [(a, BoundId)]
toList = Map.toList

fromList :: Ord a => [(a, BoundId)] -> FreeEnv a
fromList = Map.fromList

insert :: Ord a => a -> BoundId -> FreeEnv a -> FreeEnv a
insert = Map.insert

insertLM :: (MonadReader r m, Ord a) => FreeEnvLens a r -> a -> BoundId -> m c -> m c
insertLM l a b = local (over l (insert a b))

insertM :: (MonadReader (FreeEnv a) m, Ord a) => a -> BoundId -> m c -> m c
insertM = insertLM id

insertAll :: (Foldable f, Ord a) => f (a, BoundId) -> FreeEnv a -> FreeEnv a
insertAll ps m = foldr (\(a, b) n -> insert a b n) m ps

insertAllLM :: (MonadReader r m, Foldable f, Ord a) => FreeEnvLens a r -> f (a, BoundId) -> m c -> m c
insertAllLM l ps = local (over l (insertAll ps))

insertAllM :: (MonadReader (FreeEnv a) m, Foldable f, Ord a) => f (a, BoundId) -> m c -> m c
insertAllM = insertAllLM id

lookup :: Ord a => a -> FreeEnv a -> Maybe BoundId
lookup = Map.lookup

lookupLM :: (MonadReader r m, Ord a) => FreeEnvLens a r -> a -> m (Maybe BoundId)
lookupLM l a = fmap (lookup a) (view l)

lookupM :: (MonadReader (FreeEnv a) m, Ord a) => a -> m (Maybe BoundId)
lookupM = lookupLM id

newtype FreeEnvMissingErr a = FreeEnvMissingErr { unFreeEnvMissingErr :: a }
  deriving newtype (Eq, Ord, Typeable)
  deriving stock (Show)

instance (Show a, Typeable a) => Exception (FreeEnvMissingErr a)
