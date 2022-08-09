module Uniter.Reunitable.Class
  ( Reunitable (..)
  ) where

import Uniter.Core (BoundId)
import Uniter.Reunitable.Monad (ReuniterM)

class (Traversable f, Traversable g, Traversable h) => Reunitable f h e u g | f -> h e u g where
  reunite :: f (ReuniterM e u g (BoundId, h BoundId)) -> ReuniterM e u g (BoundId, h BoundId)
