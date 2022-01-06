{-# LANGUAGE UndecidableInstances #-}

module Uniter.Halt
  ( MonadHalt (..)
  , Halt (..)
  , IOE
  , runIOE
  ) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Strict (WriterT)

-- | Only the throw part of MonadError
class Monad m => MonadHalt e m | m -> e where
  halt :: e -> m a

instance Monad m => MonadHalt e (ExceptT e m) where
  halt = throwError

instance MonadHalt e m => MonadHalt e (ReaderT r m) where
  halt = lift . halt

instance MonadHalt e m => MonadHalt e (StateT s m) where
  halt = lift . halt

instance (Monoid w, MonadHalt e m) => MonadHalt e (WriterT w m) where
  halt = lift . halt

newtype IOE e a = IOE { unIOE :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runIOE :: Exception e => IOE e a -> IO (Either e a)
runIOE = try . unIOE

instance Exception e => MonadHalt e (IOE e) where
  halt = IOE . throwIO

-- | A wrapper to derive via through MonadError
newtype Halt e m a = Halt { unHalt :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadError e m => MonadHalt e (Halt e m) where
  halt = Halt . throwError
