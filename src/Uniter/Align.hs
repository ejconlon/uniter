module Uniter.Align
  ( UnalignableError (..)
  , Alignable (..)
  ) where

import Control.Exception (Exception)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.These (These (..), mergeThese)
import Data.Typeable (Typeable)

data UnalignableError = UnalignableError
  deriving stock (Eq, Show, Typeable)

instance Exception UnalignableError

-- TODO use Semialign from semialign package
class Traversable f => Alignable e f | f -> e where
  alignWith :: (These a b -> c) -> f a -> f b -> Either e (f c)
  alignWith f fa fb = fmap (fmap f) (align fa fb)

  align :: f a -> f b -> Either e (f (These a b))
  align = alignWith id

  alignAll :: (These b a -> b) -> [f a] -> Either (Maybe e) (f b)
  alignAll f = go (Left Nothing) where
    go acc = \case
      [] -> acc
      fa:fas ->
        case acc of
          Right gz ->
            case alignWith f gz fa of
              Left e -> Left (Just e)
              Right pza -> go (Right pza) fas
          _ -> go (Right (fmap (f . That) fa)) fas

  {-# MINIMAL alignWith | align #-}
