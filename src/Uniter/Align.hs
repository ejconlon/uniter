module Uniter.Align
  ( UnalignableError (..)
  , Alignable (..)
  ) where

import Control.Exception (Exception)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)

data UnalignableError = UnalignableError
  deriving stock (Eq, Show, Typeable)

instance Exception UnalignableError

-- TODO use Semialign from semialign package
class Traversable f => Alignable e f | f -> e where
  alignWith :: (a -> b -> c) -> f a -> f b -> Either e (f c)
  alignWith f fa fb = fmap (fmap (uncurry f)) (align fa fb)

  align :: f a -> f b -> Either e (f (a, b))
  align = alignWith (,)

  alignAll :: [f a] -> Either (Maybe e) (f (Seq a))
  alignAll = go (Left Nothing) where
    go acc = \case
      [] -> acc
      fa:fas ->
        case acc of
          Right gz ->
            case alignWith (:|>) gz fa of
              Left e -> Left (Just e)
              Right pza -> go (Right pza) fas
          _ -> go (Right (fmap Seq.singleton fa)) fas

  {-# MINIMAL alignWith | align #-}
