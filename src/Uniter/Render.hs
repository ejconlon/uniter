{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Uniter.Render
  ( renderDot
  , renderDoc
  , boundEnvGraph
  ) where

import Algebra.Graph (Graph, edge, vertex)
import qualified Algebra.Graph.Export as AGE
import qualified Algebra.Graph.Export.Dot as AGED
import Data.Foldable (toList)
import Uniter.Core (BoundId (..))
import Uniter.Graph (BoundEnv, Elem (..), lookupBoundEnv, toListBoundEnv)

reprBoundId :: BoundId -> String
reprBoundId = show . unBoundId

reprElem :: (Show (f Int), Functor f) => Elem f -> String
reprElem = \case
  ElemNode n -> unwords ["Node", show (fmap unBoundId n)]
  ElemEq x y -> unwords ["Eq", reprBoundId x, reprBoundId y]
  ElemFresh -> "Fresh"

elemChildren :: Foldable f => Elem f -> [BoundId]
elemChildren = \case
  ElemNode z -> toList z
  ElemEq x y -> [x, y]
  ElemFresh -> []

boundEnvGraph :: Foldable f => BoundEnv f -> Graph BoundId
boundEnvGraph = foldMap go . toListBoundEnv where
  go (b, e) = vertex b <> foldMap (edge b) (elemChildren e)

reprInfo :: (Show (f Int), Functor f) => BoundId -> Elem f -> String
reprInfo b e = show (unBoundId b) ++ ": " ++ reprElem e

reprLookup :: (Show (f Int), Functor f) => BoundEnv f -> BoundId -> String
reprLookup x b = case lookupBoundEnv b x of
  Nothing -> error ("Missing " ++ show b)
  Just e -> reprInfo b e

renderDot :: (Show (f Int), Functor f, Foldable f) => BoundEnv f -> String
renderDot x =
  let g = boundEnvGraph x
      s = AGED.defaultStyle reprBoundId
      t = s { AGED.vertexAttributes = \a -> ["label" AGED.:= reprLookup x a] }
  in AGED.export t g

renderDoc :: (Show (f Int), Functor f, Foldable f) => BoundEnv f -> String
renderDoc x =
  let g = boundEnvGraph x
      vDoc a = AGE.literal (reprLookup x a) <> "\n"
      eDoc a b = AGE.literal (reprBoundId a) <> " -> " <> AGE.literal (reprBoundId b) <> "\n"
  in AGE.render (AGE.export vDoc eDoc g)
