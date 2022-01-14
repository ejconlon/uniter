module Uniter.Interface
  ( RebindMap
  , initialGraph
  , processGraph
  , writeDotGraph
  ) where

import Control.Monad.Trans.State.Strict (runState)
import Data.Functor.Foldable (Base, Recursive)
import IntLike.Map (IntLikeMap)
import Uniter.Align (Alignable)
import Uniter.Core (BoundId (..), Unitable, handleEvents, uniteTerm)
import Uniter.Graph (BoundEnv (..), Elem (..), GraphState (..), appUniter, newGraphState, runAppM, streamBoundEnv)
import Uniter.Process (Defn (..), ProcessError, ProcessState (..), defnTraversal, newProcessState, psUnionMapL,
                       runProcessM)
import Uniter.Render (renderDot)
import Uniter.UnionMap (UnionEntry (..), UnionMap (..), canonicalizeUnionMapLM, compactUnionMapLM)

type RebindMap = IntLikeMap BoundId BoundId

initialGraph :: (Recursive t, Base t ~ f, Unitable v e g f) => t -> v -> (Either e BoundId, GraphState g)
initialGraph t v =
  let m = appUniter (uniteTerm t)
  in runAppM m v (newGraphState (BoundId 0))

compactProcess :: ProcessState g -> (RebindMap, ProcessState g)
compactProcess = runState (compactUnionMapLM psUnionMapL)

canonicalizeProcess :: Traversable g => ProcessState g -> (RebindMap, ProcessState g)
canonicalizeProcess = runState (canonicalizeUnionMapLM psUnionMapL defnTraversal)

extractProcess :: Traversable g => ProcessState g -> (RebindMap, GraphState g)
extractProcess ps = res where
  res =
    let (m, ProcessState i u) = canonicalizeProcess ps
        b = go1 u
    in (m, GraphState i b)
  go1 = BoundEnv . fmap go2 . unUnionMap
  go2 = \case
    UnionEntryLink _ -> error "impossible"
    UnionEntryValue d ->
      case d of
        DefnFresh -> ElemFresh
        DefnNode n -> ElemNode n

processGraph :: Alignable e g => GraphState g -> Either (ProcessError e) (RebindMap, GraphState g)
processGraph (GraphState i b) =
  let m = handleEvents (streamBoundEnv b)
      (eu, ps) = runProcessM m (newProcessState i)
  in case eu of
    Left e -> Left e
    Right _ -> Right (extractProcess ps)

writeDotGraph :: (Show (g Int), Functor g, Foldable g) => FilePath -> GraphState g -> IO ()
writeDotGraph p (GraphState _ b) = writeFile p (renderDot b)
