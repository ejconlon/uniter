module Uniter.Align
  ( UnalignableError (..)
  , Alignable (..)
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data UnalignableError = UnalignableError
  deriving stock (Eq, Show, Typeable)

instance Exception UnalignableError

class Traversable f => Alignable e f | f -> e where
  align :: f a -> f b -> Either e (f (a, b))

  alignAll :: [f a] -> Either (Maybe e) (f [a])
  alignAll = fmap (fmap reverse) . go (Left Nothing) where
    go acc = \case
      [] -> acc
      fa:fas ->
        case acc of
          Right gz ->
            case align gz fa of
              Left e -> Left (Just e)
              Right pza -> go (Right (fmap (\(as, a) -> a:as) pza)) fas
          _ -> go (Right (fmap pure fa)) fas
