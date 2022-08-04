module Uniter.Interface
  ( RebindMap
  , UniterErr (..)
  , uniteResult
  , quickUniteResult
  , initialGraph
  , processGraph
  , extractResult
  , writeDotGraph
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State.Strict (evalState)
import Control.Monad.Trans.State.Strict (runState)
import Data.Functor.Foldable (Base, Corecursive, Recursive)
import Data.Typeable (Typeable)
import IntLike.Map (IntLikeMap)
import Uniter.Align (Alignable)
import Uniter.Core (BoundId (..), Unitable, handleEvents, uniteTerm)
import Uniter.Graph (BoundEnv (..), Elem (..), GraphState (..), appUniter, graphResolveVar, newGraphState, runAppM,
                     streamBoundEnv)
import Uniter.Process (Defn (..), ProcessErr, ProcessState (..), defnTraversal, newProcessState, psUnionMapL,
                       runProcessM)
import Uniter.Render (renderDot)
import Uniter.UnionMap (UnionEntry (..), UnionMap (..), canonicalizeUnionMapLM, compactUnionMapLM)

type RebindMap = IntLikeMap BoundId BoundId

data UniterErr ue ae =
    UniterErrGraph !ue
  | UniterErrProcess !(ProcessErr ae)
  | UniterErrExtract !BoundId
  deriving stock (Eq, Ord, Show)

instance (Show ue, Typeable ue, Show ae, Typeable ae) => Exception (UniterErr ue ae)

-- | Perform unification on a term in one go.
-- NOTE It may be helpful to alias this function with the types filled in.
uniteResult :: (Recursive t, Base t ~ f, Unitable v ue g f, Corecursive u, Base u ~ g, Alignable ae g) => t -> v -> (Either (UniterErr ue ae) u, GraphState g)
uniteResult t v =
  let (eb, ig) = initialGraph t v
  in case eb of
    Left err -> (Left (UniterErrGraph err), ig)
    Right bid ->
      case processGraph ig of
        Left err -> (Left (UniterErrProcess err), ig)
        Right (_, pg) -> do
          case extractResult bid pg of
            Left mid -> (Left (UniterErrExtract mid), pg)
            Right ty -> (Right ty, pg)

quickUniteResult :: (Monoid v, MonadThrow m, Recursive t, Base t ~ f, Unitable v ue g f, Corecursive u, Base u ~ g, Alignable ae g, Show ue, Show ae, Typeable ue, Typeable ae) => t -> m u
quickUniteResult t =
  let (eu, _) = uniteResult t mempty
  in either throwM pure eu

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

processGraph :: Alignable e g => GraphState g -> Either (ProcessErr e) (RebindMap, GraphState g)
processGraph (GraphState i b) =
  let m = handleEvents (streamBoundEnv b)
      (eu, ps) = runProcessM m (newProcessState i)
  in case eu of
    Left e -> Left e
    Right _ -> Right (extractProcess ps)

extractResult :: (Corecursive u, Base u ~ g, Traversable g) => BoundId -> GraphState g -> Either BoundId u
extractResult = evalState . graphResolveVar

writeDotGraph :: (Show (g Int), Functor g, Foldable g) => FilePath -> GraphState g -> IO ()
writeDotGraph p (GraphState _ b) = writeFile p (renderDot b)
