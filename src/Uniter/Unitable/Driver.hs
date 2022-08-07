module Uniter.Unitable.Driver
  ( UniteErr (..)
  , UniteSuccess (..)
  , UniteResult
  , uniteResult
  , quickUniteResult
  , driveUniteResult
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.Functor.Foldable (Base, Corecursive, Recursive)
import Data.Typeable (Typeable)
import Uniter.Align (Alignable)
import Uniter.Core (BoundId (..), Node)
import Uniter.Graph (Graph, resolveVar)
import Uniter.Monad (UniterT, newUniterState, preGraph, runUniterT)
import Uniter.PreGraph (PreGraph (..))
import Uniter.Process (ProcessErr, RebindMap, embedUniterT, extract, newProcessState, runProcessT)
import Uniter.Unitable.Class (Unitable, uniteTerm)

data UniteErr e g =
    UniteErrProcess !(ProcessErr e)
  | UniteErrExtract !BoundId !BoundId !RebindMap !(Graph g)

deriving instance (Eq e, Eq (Node g)) => Eq (UniteErr e g)
deriving instance (Show e, Show (Node g)) => Show (UniteErr e g)

instance (Show e, Show (Node g), Typeable e, Typeable g) => Exception (UniteErr e g)

-- data UniteMetaErr e g =
--     UniteMetaErrAlign !e
--   | UniteMetaErrDrive !(UniteErr e g)

-- deriving instance (Eq e, Eq (Node g)) => Eq (UniteMetaErr e g)
-- deriving instance (Show e, Show (Node g)) => Show (UniteMetaErr e g)

-- instance (Show e, Show (Node g), Typeable e, Typeable g) => Exception (UniteMetaErr e g)

data UniteSuccess g u = UniteSuccess !BoundId !u !RebindMap !(Graph g)
  deriving stock (Functor, Foldable, Traversable)

deriving instance (Eq (Node g), Eq u) => Eq (UniteSuccess g u)
deriving instance (Show (Node g), Show u) => Show (UniteSuccess g u)

type UniteResult e g u = Either (UniteErr e g) (UniteSuccess g u)

-- | Perform unification on a term in one go. -- NOTE It may be helpful to alias this function with the types filled in.
uniteResult :: (Recursive t, Base t ~ f, Unitable f g m, Corecursive u, Base u ~ g, Alignable e g) => t -> m (PreGraph g, UniteResult e g u)
uniteResult = driveUniteResult . uniteTerm

quickUniteResult :: (Recursive t, Base t ~ f, Unitable f g m, Corecursive u, Base u ~ g, Alignable e g, MonadThrow m, Show e, Show (Node g), Typeable e, Typeable g) => t -> m u
quickUniteResult t = do
  r <- fmap snd (uniteResult t)
  case r of
    Left e -> throwM e
    Right (UniteSuccess _ u _ _) -> pure u

driveUniteResult :: (Monad m, Corecursive u, Base u ~ g, Alignable e g) => UniterT g m BoundId -> m (PreGraph g, UniteResult e g u)
driveUniteResult act = do
  let uniq = toEnum 0
  pg <- fmap fst (runUniterT (act *> preGraph) (newUniterState uniq))
  (ea, _) <- flip runProcessT (newProcessState uniq) $ do
    bid <- embedUniterT act
    (rebinds, graph) <- extract
    pure (bid, rebinds, graph)
  let res = case ea of
        Left pe -> Left (UniteErrProcess pe)
        Right (bid, rebinds, graph) ->
          case resolveVar bid graph of
            Left xid -> Left (UniteErrExtract bid xid rebinds graph)
            Right u -> Right (UniteSuccess bid u rebinds graph)
  pure (pg, res)

