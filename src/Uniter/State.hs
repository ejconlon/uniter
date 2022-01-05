module Uniter.State
  ( mayStateLens
  , stateLens
  ) where

import Control.Monad.State.Strict (MonadState, state)
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
