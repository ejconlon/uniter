module Uniter.Halt
  ( MonadHalt (..)
  , Halt (..)
  ) where

import Control.Monad.Except (ExceptT, MonadError (..))

-- | Only the throw part of MonadError
class Monad m => MonadHalt e m | m -> e where
  halt :: e -> m a

instance Monad m => MonadHalt e (ExceptT e m) where
  halt = throwError

-- | A wrapper to derive via through MonadError
newtype Halt e m a = Halt { unHalt :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadError e m => MonadHalt e (Halt e m) where
  halt = Halt . throwError
