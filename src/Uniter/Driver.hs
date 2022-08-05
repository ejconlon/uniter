module Uniter.Driver
  ( ExtractErr (..)
  , UniteResult (..)
  , uniteResult
  , quickUniteResult
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.Functor.Foldable (Base, Corecursive, Recursive)
import Data.Typeable (Typeable)
import Uniter.Align (Alignable)
import Uniter.Core (BoundId (..), Node, Unitable, uniteTerm)
import Uniter.Graph (Graph, resolveVar)
import Uniter.Process (ProcessErr, RebindMap, embedUniterM, extract, newProcessState, runProcessT)

data ExtractErr = ExtractErr !BoundId !BoundId
  deriving stock (Eq, Ord, Show)

instance Exception ExtractErr

data UniteResult e g u =
    UniteResultProcessErr !(ProcessErr e)
  | UniteResultExtractErr !BoundId !BoundId !RebindMap !(Graph g)
  | UniteResultSuccess !BoundId !u !RebindMap !(Graph g)

deriving instance (Eq e, Eq (Node g), Eq u) => Eq (UniteResult e g u)
deriving instance (Show e, Show (Node g), Show u) => Show (UniteResult e g u)

-- | Perform unification on a term in one go. -- NOTE It may be helpful to alias this function with the types filled in.
uniteResult :: (Recursive t, Base t ~ f, Unitable g f m, Corecursive u, Base u ~ g, Alignable e g) => t -> m (UniteResult e g u)
uniteResult t = do
  (ea, _) <- flip runProcessT (newProcessState (toEnum 0)) $ do
    bid <- embedUniterM (uniteTerm t)
    (rebinds, graph) <- extract
    pure (bid, rebinds, graph)
  pure $! case ea of
    Left pe -> UniteResultProcessErr pe
    Right (bid, rebinds, graph) ->
      case resolveVar bid graph of
        Left xid -> UniteResultExtractErr bid xid rebinds graph
        Right u -> UniteResultSuccess bid u rebinds graph

quickUniteResult :: (Recursive t, Base t ~ f, Unitable g f m, Corecursive u, Base u ~ g, Alignable e g, MonadThrow m, Show e, Typeable e) => t -> m u
quickUniteResult t = do
  r <- uniteResult t
  case r of
    UniteResultProcessErr pe -> throwM pe
    UniteResultExtractErr bid xid _ _ -> throwM (ExtractErr bid xid)
    UniteResultSuccess _ u _ _ -> pure u
