{-# LANGUAGE UndecidableInstances #-}

module Uniter.Reunitable.Driver
  ( ReuniteErr (..)
  , ReuniteSuccess (..)
  , ReuniteResult
  , reuniteResult
  , quickReuniteResult
  , driveReuniteResult
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.Bitraversable (Bitraversable)
import Data.Either (fromRight)
import Data.Functor.Foldable (Base, Corecursive, Recursive)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Uniter.Align (Alignable)
import Uniter.Core (Node, SpecTm, UniqueId)
import Uniter.Graph (Graph, resolveTm, resolveVar)
import Uniter.PreGraph (PreGraph (..))
import Uniter.Process (ProcessErr, embedReuniterM, extract, newProcessState, runProcessM)
import Uniter.Reunitable.Class (Reunitable, reuniteTerm)
import Uniter.Reunitable.Monad (ReuniterM, newReuniterEnv, newReuniterState, preGraph, runReuniterM)

data ReuniteErr e h g u =
    ReuniteErrProcess !(ProcessErr e g)
  | ReuniteErrExtractTy !UniqueId !(SpecTm h UniqueId) !UniqueId !(Graph g)
  -- ^ (id of type, reconstructed term, id of failed extract, graph)
  | ReuniteErrExtractTm !UniqueId !(SpecTm h UniqueId) !u !UniqueId !(Graph g)
  -- ^ (id of type, reconstructed term, id of failed extract, graph)

deriving instance (Eq e, Eq (Node g), Eq (h UniqueId (SpecTm h UniqueId)), Eq u) => Eq (ReuniteErr e h g u)
deriving instance (Show e, Show (Node g), Show (h UniqueId (SpecTm h UniqueId)), Show u) => Show (ReuniteErr e h g u)

instance (Show e, Show (Node g), Show (h UniqueId (SpecTm h UniqueId)), Show u, Typeable e, Typeable h, Typeable g, Typeable u) => Exception (ReuniteErr e h g u)

data ReuniteSuccess h g u = ReuniteSuccess !UniqueId !(SpecTm h u) !u !(Graph g)
  deriving stock (Functor, Foldable, Traversable)

deriving instance (Eq (Node g), Eq u, Eq (h u (SpecTm h u))) => Eq (ReuniteSuccess h g u)
deriving instance (Show (Node g), Show u, Show (h u (SpecTm h u))) => Show (ReuniteSuccess h g u)

type ReuniteResult e h g u = Either (ReuniteErr e h g u) (ReuniteSuccess h g u)

-- | Perform unification on a term in one go. -- NOTE It may be helpful to alias this function with the types filled in.
reuniteResult :: (Recursive t, Base t ~ f, Reunitable f h g, Corecursive u, Base u ~ g, Alignable e g) => t -> (PreGraph g, ReuniteResult e h g u)
reuniteResult = driveReuniteResult . reuniteTerm

quickReuniteResult ::
  (Recursive t, Base t ~ f, Reunitable f h g, Corecursive u, Base u ~ g, Alignable e g, MonadThrow m,
  Show e, Show (Node g), Show u, Show (h UniqueId (SpecTm h UniqueId)), Typeable e, Typeable g, Typeable h, Typeable u) => t -> m (SpecTm h u, u)
quickReuniteResult t =
  let r = snd (reuniteResult t)
  in case r of
    Left e -> throwM e
    Right (ReuniteSuccess _ tm u _) -> pure (tm, u)

driveReuniteResult :: (Bitraversable h, Corecursive u, Base u ~ g, Alignable e g) => ReuniterM g (UniqueId, SpecTm h UniqueId) -> (PreGraph g, ReuniteResult e h g u)
driveReuniteResult act =
  let fm = Map.empty
      uniq = toEnum 0
      pg = fromRight (error "impossible") (fst (runReuniterM (act *> preGraph) (newReuniterEnv fm) (newReuniterState uniq)))
      (ea, _) = flip runProcessM (newProcessState uniq) $ do
        bid <- embedReuniterM fm act
        (_, graph) <- extract
        pure (bid, graph)
      res = case ea of
        Left pe -> Left (ReuniteErrProcess pe)
        Right ((bid, tm), graph) ->
          case resolveVar bid graph of
            Left xid -> Left (ReuniteErrExtractTy bid tm xid graph)
            Right u ->
              case resolveTm tm graph of
                Left xid -> Left (ReuniteErrExtractTm bid tm u xid graph)
                Right tm' -> Right (ReuniteSuccess bid tm' u graph)
  in (pg, res)
