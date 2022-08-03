module Uniter.Align
  ( UnalignableError (..)
  , Alignable (..)
  ) where

import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.These (These (..))
import Data.Typeable (Typeable)

-- | You can just use this error if you don't feel like reporting more when alignment fails.
data UnalignableError = UnalignableError
  deriving stock (Eq, Show, Typeable)

instance Exception UnalignableError

-- | Aligns the holes of compatible structures
class Traversable f => Alignable e f | f -> e where
  -- | If the two structures are alignable, return a structure filled with shared parts.
  -- Law: A structure must align with itself in the natural way.
  align :: f a -> f b -> Either e (f (These a b))
  align = alignWith id

  -- | 'align' but saving you an 'fmap' to eliminate 'These'.
  alignWith :: (These a b -> c) -> f a -> f b -> Either e (f c)
  alignWith f fa fb = fmap (fmap f) (align fa fb)

  alignAll :: Foldable t => (These z a -> z) -> t (f a) -> Either (Maybe e) (f z)
  alignAll f = go (Left Nothing) . toList where
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
