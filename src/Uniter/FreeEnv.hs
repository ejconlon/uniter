module Uniter.FreeEnv
  ( FreeName (..)
  , FreeEnv (..)
  , FreeEnvLens
  , emptyFreeEnv
  , insertFreeEnv
  , insertFreeEnvLM
  , insertFreeEnvM
  , lookupFreeEnv
  , lookupFreeEnvLM
  , lookupFreeEnvM
  , FreeEnvMissingError (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader (..))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Lens.Micro (Lens', over)
import Lens.Micro.Mtl (view)
import Uniter.Core (BoundId)

newtype FreeName = FreeName { unFreeName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, NFData, IsString)

newtype FreeEnv = FreeEnv { unFreeEnv :: Map FreeName BoundId }
  deriving stock (Show)
  deriving newtype (Eq)

type FreeEnvLens s = Lens' s FreeEnv

emptyFreeEnv :: FreeEnv
emptyFreeEnv = FreeEnv Map.empty

insertFreeEnv :: FreeName -> BoundId -> FreeEnv -> FreeEnv
insertFreeEnv a b (FreeEnv m) = FreeEnv (Map.insert a b m)

insertFreeEnvLM :: MonadReader r m => FreeEnvLens r -> FreeName -> BoundId -> m a -> m a
insertFreeEnvLM l a b = local (over l (insertFreeEnv a b))

insertFreeEnvM :: MonadReader FreeEnv m => FreeName -> BoundId -> m a -> m a
insertFreeEnvM = insertFreeEnvLM id

lookupFreeEnv :: FreeName -> FreeEnv -> Maybe BoundId
lookupFreeEnv a (FreeEnv m) = Map.lookup a m

lookupFreeEnvLM :: MonadReader r m => FreeEnvLens r -> FreeName -> m (Maybe BoundId)
lookupFreeEnvLM l a = fmap (lookupFreeEnv a) (view l)

lookupFreeEnvM :: MonadReader FreeEnv m => FreeName -> m (Maybe BoundId)
lookupFreeEnvM = lookupFreeEnvLM id

newtype FreeEnvMissingError = FreeEnvMissingError { unFreeEnvMissingError :: FreeName }
  deriving newtype (Eq, Typeable)
  deriving stock (Show)

instance Exception FreeEnvMissingError
