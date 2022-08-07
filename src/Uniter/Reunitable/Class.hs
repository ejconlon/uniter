{-# LANGUAGE UndecidableInstances #-}

module Uniter.Reunitable.Class
  ( Reunion (..)
  , Reunitable (..)
  ) where

import Uniter.Core (BoundId)
import Uniter.Monad (UniterT)

data Reunion h = Reunion
  { reunionTy :: !BoundId
  , reunionTm :: !(h BoundId)
  }

deriving instance Eq (h BoundId) => Eq (Reunion h)
deriving instance Show (h BoundId) => Show (Reunion h)

class (Traversable f, Traversable g, Traversable h, Monad m) => Reunitable f g h m | f -> g h m where
  reunite :: f (UniterT g m (Reunion h)) -> UniterT g m (Reunion h)
