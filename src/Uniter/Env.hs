module Uniter.Env
  ( FreeName (..)
  , FreeEnv (..)
  , emptyFreeEnv
  , insertFreeEnv
  , insertFreeEnvM
  , lookupFreeEnv
  , lookupFreeEnvM
  , FreeEnvMissingError (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Uniter.Core (BoundId)

newtype FreeName = FreeName { unFreeName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, NFData, IsString)

newtype FreeEnv = FreeEnv { unFreeEnv :: Map FreeName BoundId }
  deriving stock (Show)
  deriving newtype (Eq)

emptyFreeEnv :: FreeEnv
emptyFreeEnv = FreeEnv Map.empty

insertFreeEnv :: FreeName -> BoundId -> FreeEnv -> FreeEnv
insertFreeEnv a b (FreeEnv m) = FreeEnv (Map.insert a b m)

insertFreeEnvM :: MonadReader FreeEnv m => FreeName -> BoundId -> m a -> m a
insertFreeEnvM a b = local (insertFreeEnv a b)

lookupFreeEnv :: FreeName -> FreeEnv -> Maybe BoundId
lookupFreeEnv a (FreeEnv m) = Map.lookup a m

lookupFreeEnvM :: MonadReader FreeEnv m => FreeName -> m (Maybe BoundId)
lookupFreeEnvM a = asks (lookupFreeEnv a)

newtype FreeEnvMissingError = FreeEnvMissingError { unFreeEnvMissingError :: FreeName }
  deriving newtype (Eq, Typeable)
  deriving stock (Show)

instance Exception FreeEnvMissingError
