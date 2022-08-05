{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Uniter.Render
  ( renderDot
  , renderDoc
  , writeDotGraph
  ) where

import qualified Algebra.Graph as AG
import qualified Algebra.Graph.Export as AGE
import qualified Algebra.Graph.Export.Dot as AGED
import Data.Foldable (toList)
import Uniter.Core (BoundId (..))
import Uniter.Graph (Elem (..), Graph)
import qualified Uniter.Graph as UG

reprBoundId :: BoundId -> String
reprBoundId = show . unBoundId

reprElem :: (Show (g Int), Functor g) => Elem g -> String
reprElem = \case
  ElemNode n -> unwords ["Node", show (fmap unBoundId n)]
  ElemFresh -> "Fresh"

elemChildren :: Foldable g => Elem g -> [BoundId]
elemChildren = \case
  ElemNode z -> toList z
  ElemFresh -> []

xformGraph :: Foldable g => Graph g -> AG.Graph BoundId
xformGraph = foldMap go . UG.toList where
  go (b, e) = AG.vertex b <> foldMap (AG.edge b) (elemChildren e)

reprInfo :: (Show (g Int), Functor g) => BoundId -> Elem g -> String
reprInfo b e = show (unBoundId b) ++ ": " ++ reprElem e

reprLookup :: (Show (g Int), Functor g) => Graph g -> BoundId -> String
reprLookup x b = case UG.lookup b x of
  Nothing -> error ("Missing " ++ show b)
  Just e -> reprInfo b e

renderDot :: (Show (g Int), Functor g, Foldable g) => Graph g -> String
renderDot x =
  let g = xformGraph x
      s = AGED.defaultStyle reprBoundId
      t = s { AGED.vertexAttributes = \a -> ["label" AGED.:= reprLookup x a] }
  in AGED.export t g

renderDoc :: (Show (g Int), Functor g, Foldable g) => Graph g -> String
renderDoc x =
  let g = xformGraph x
      vDoc a = AGE.literal (reprLookup x a) <> "\n"
      eDoc a b = AGE.literal (reprBoundId a) <> " -> " <> AGE.literal (reprBoundId b) <> "\n"
  in AGE.render (AGE.export vDoc eDoc g)

writeDotGraph :: (Show (g Int), Functor g, Foldable g) => FilePath -> Graph g -> IO ()
writeDotGraph fp g = writeFile fp (renderDot g)
