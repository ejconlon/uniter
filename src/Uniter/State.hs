module Uniter.State
  ( mayStateLens
  , stateLens
  , IterM
  , runIterM
  ) where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (MonadState, StateT, runStateT, state)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)
import Uniter.Halt (MonadHalt (..))

mayStateLens :: MonadState s m => Lens' s x -> (x -> (a, Maybe x)) -> m a
mayStateLens l f = state $ \s ->
  let u = view l s
      (a, mu') = f u
  in case mu' of
    Nothing -> (a, s)
    Just u' ->
      let s' = set l u' s
      in (a, s')

stateLens :: MonadState s m => Lens' s x -> (x -> (a, x)) -> m a
stateLens l f = state $ \s ->
  let u = view l s
      (a, u') = f u
      s' = set l u' s
  in (a, s')

newtype IterM e s a = IterM { unIterM :: StateT s (Except e) a }
  deriving newtype (Functor, Applicative, Monad, MonadState s)

runIterM :: IterM e s a -> s -> Either e (a, s)
runIterM it s = runExcept (runStateT (unIterM it) s)

instance MonadHalt e (IterM e s) where
  halt = IterM . throwError
