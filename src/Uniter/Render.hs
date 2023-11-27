{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Uniter.Render
  ( renderGraphDot
  , renderGraphDoc
  , writeGraphDot
  , renderPreGraphDot
  , renderPreGraphDoc
  , writePreGraphDot
  )
where

import qualified Algebra.Graph as AG
import qualified Algebra.Graph.Export as AGE
import qualified Algebra.Graph.Export.Dot as AGED
import Data.Foldable (toList)
import qualified Data.Text as T
import Uniter.Core (TyBinder (..), TyVar (..), UniqueId (..))
import Uniter.Graph (Elem (..), Graph)
import qualified Uniter.Graph as UG
import Uniter.PreGraph (PreElem (..), PreGraph)
import qualified Uniter.PreGraph as UP

reprUniqueId :: UniqueId -> String
reprUniqueId = show . unUniqueId

reprElem :: (Show (g Int), Functor g) => Elem g -> String
reprElem = \case
  ElemNode n -> unwords ["Node", show (fmap unUniqueId n)]
  ElemMeta tyb -> maybe "Meta" (\(TyVar n) -> "Meta " ++ T.unpack n) (unTyBinder tyb)
  ElemSkolem tyb -> maybe "Skolem" (\(TyVar n) -> "Skolem " ++ T.unpack n) (unTyBinder tyb)

reprPreElem :: (Show (g Int), Functor g) => PreElem g -> String
reprPreElem = \case
  PreElemNode n -> unwords ["Node", show (fmap unUniqueId n)]
  PreElemEq i j -> unwords ["Eq", reprUniqueId i, reprUniqueId j]
  PreElemMeta tyb -> maybe "Meta" (\(TyVar n) -> "Meta " ++ T.unpack n) (unTyBinder tyb)
  PreElemSkolem tyb -> maybe "Skolem" (\(TyVar n) -> "Skolem " ++ T.unpack n) (unTyBinder tyb)

elemChildren :: (Foldable g) => Elem g -> [UniqueId]
elemChildren = \case
  ElemNode z -> toList z
  _ -> []

preElemChildren :: (Foldable g) => PreElem g -> [UniqueId]
preElemChildren = \case
  PreElemNode z -> toList z
  PreElemEq i j -> [i, j]
  _ -> []

xformGraph :: (Foldable g) => Graph g -> AG.Graph UniqueId
xformGraph = foldMap go . UG.toList
 where
  go (b, e) = AG.vertex b <> foldMap (AG.edge b) (elemChildren e)

xformPreGraph :: (Foldable g) => PreGraph g -> AG.Graph UniqueId
xformPreGraph = foldMap go . UP.toList
 where
  go (b, e) = AG.vertex b <> foldMap (AG.edge b) (preElemChildren e)

reprInfo :: (Show (g Int), Functor g) => UniqueId -> Elem g -> String
reprInfo b e = reprUniqueId b ++ ": " ++ reprElem e

reprPreInfo :: (Show (g Int), Functor g) => UniqueId -> PreElem g -> String
reprPreInfo b e = reprUniqueId b ++ ": " ++ reprPreElem e

reprLookup :: (Show (g Int), Functor g) => Graph g -> UniqueId -> String
reprLookup x b = case UG.lookup b x of
  Nothing -> error ("Missing " ++ show b)
  Just e -> reprInfo b e

reprPreLookup :: (Show (g Int), Functor g) => PreGraph g -> UniqueId -> String
reprPreLookup x b = case UP.lookup b x of
  Nothing -> error ("Missing " ++ show b)
  Just e -> reprPreInfo b e

renderGraphDot :: (Show (g Int), Functor g, Foldable g) => Graph g -> String
renderGraphDot x =
  let g = xformGraph x
      s = AGED.defaultStyle reprUniqueId
      t = s {AGED.vertexAttributes = \a -> ["label" AGED.:= reprLookup x a]}
  in  AGED.export t g

renderGraphDoc :: (Show (g Int), Functor g, Foldable g) => Graph g -> String
renderGraphDoc x =
  let g = xformGraph x
      vDoc a = AGE.literal (reprLookup x a) <> "\n"
      eDoc a b = AGE.literal (reprUniqueId a) <> " -> " <> AGE.literal (reprUniqueId b) <> "\n"
  in  AGE.render (AGE.export vDoc eDoc g)

writeGraphDot :: (Show (g Int), Functor g, Foldable g) => FilePath -> Graph g -> IO ()
writeGraphDot fp g = writeFile fp (renderGraphDot g)

renderPreGraphDot :: (Show (g Int), Functor g, Foldable g) => PreGraph g -> String
renderPreGraphDot x =
  let g = xformPreGraph x
      s = AGED.defaultStyle reprUniqueId
      t = s {AGED.vertexAttributes = \a -> ["label" AGED.:= reprPreLookup x a]}
  in  AGED.export t g

renderPreGraphDoc :: (Show (g Int), Functor g, Foldable g) => PreGraph g -> String
renderPreGraphDoc x =
  let g = xformPreGraph x
      vDoc a = AGE.literal (reprPreLookup x a) <> "\n"
      eDoc a b = AGE.literal (reprUniqueId a) <> " -> " <> AGE.literal (reprUniqueId b) <> "\n"
  in  AGE.render (AGE.export vDoc eDoc g)

writePreGraphDot :: (Show (g Int), Functor g, Foldable g) => FilePath -> PreGraph g -> IO ()
writePreGraphDot fp g = writeFile fp (renderPreGraphDot g)
