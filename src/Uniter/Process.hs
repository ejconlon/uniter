{-# LANGUAGE UndecidableInstances #-}

module Uniter.Process where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State.Strict (MonadState, State)
import Data.Map.Strict (Map)
import Data.Typeable (Typeable)
import Overeasy.EquivFind (EquivFind)
import Uniter.Align (Alignable)
import Uniter.Core (BoundId, EventHandler (..), Node (..), UniterError)
import Uniter.Graph (BoundEnv)

data ProcessError e =
    ProcessErrorUniter !(UniterError e)
  | ProcessErrorMissingBound !BoundId
  deriving stock (Eq, Show, Typeable)

instance (Show e, Typeable e) => Exception (ProcessError e)

data ProcessState f = ProcessState
  { psUnique :: !BoundId
  , psEquiv :: !(EquivFind BoundId)
  -- , psFreshes :: Set BoundId
  , psNodes :: Map BoundId (Maybe (Node f))
  }
deriving stock instance Eq (f BoundId) => Eq (ProcessState f)
deriving stock instance Show (f BoundId) => Show (ProcessState f)

newtype ProcessM e f a = ProcessM
  { unProcessM :: ExceptT (ProcessError e) (State (ProcessState f)) a
  } deriving newtype (Functor, Applicative, Monad, MonadError (ProcessError e), MonadState (ProcessState f))

instance Alignable e f => EventHandler e f (ProcessM e f) where
  handleError = throwError . ProcessErrorUniter
  handleAddNode n i = undefined
  handleEmitEq i j y = undefined
  handleFresh i = undefined

-- processRun :: Alignable e f => BoundEnv f -> ProcessM e f ()
-- processRun = undefined

runProcessM :: ProcessM e f a -> ProcessState f -> Either e (a, ProcessState f)
runProcessM = undefined

