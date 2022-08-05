module Uniter.State
  ( mayStateLens
  , stateLens
  , DropM
  , runDropM
  ) where

import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.State.Strict (MonadState, StateT, runStateT, state)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)

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

newtype DropM e s a = DropM { unDropM :: StateT s (Except e) a }
  deriving newtype (Functor, Applicative, Monad, MonadState s, MonadError e)

runDropM :: DropM e s a -> s -> Either e (a, s)
runDropM it s = runExcept (runStateT (unDropM it) s)
