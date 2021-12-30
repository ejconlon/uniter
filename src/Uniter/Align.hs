module Uniter.Align
  ( Pair (..)
  , UnalignableError (..)
  , Alignable (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data Pair a b = Pair !a !b
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, NFData)

data UnalignableError = UnalignableError
  deriving stock (Eq, Show, Typeable)

instance Exception UnalignableError

class Alignable e f | f -> e where
  align :: f a -> f b -> Either e [Pair a b]
