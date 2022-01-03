module Test.Uniter.State
  ( StateT (..)
  , State
  , MonadState (..)
  , runS
  , testS
  , applyS
  , applyTestS
  ) where

import Control.Monad.State.Strict (MonadState (..), State, StateT (..), evalStateT, runState)
import Control.Monad.Trans (lift)

runS :: Monad m => s -> StateT s m () -> m ()
runS = flip evalStateT

testS :: Monad m => (s -> m a) -> StateT s m a
testS p = get >>= lift . p

applyS :: Monad m => State s a -> StateT s m a
applyS = state . runState

applyTestS :: Monad m => State s a -> (a -> s -> m b) -> StateT s m b
applyTestS act check = do
  a <- applyS act
  s <- get
  lift (check a s)
