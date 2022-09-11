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
import Data.Functor.Foldable (Base, Recursive)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Uniter.Align (Alignable)
import Uniter.Core (GenQuant, GenTy, Node, SpecTm, UniqueId)
import Uniter.Graph (ComplexResErr, Graph, resolveGenVar, resolveTm)
import Uniter.PreGraph (PreGraph (..))
import Uniter.Process (ProcessErr, embedReuniterM, extract, newProcessState, runProcessM)
import Uniter.Reunitable.Class (Reunitable, reuniteTerm)
import Uniter.Reunitable.Monad (ReuniterM, newReuniterEnv, newReuniterState, preGraph, runReuniterM)

data ReuniteErr e h g =
    ReuniteErrProcess !(ProcessErr e g)
  | ReuniteErrExtractTy !UniqueId !(SpecTm h UniqueId) !ComplexResErr !(Graph g)
  -- ^ (id of type, reconstructed term, id of failed extract, graph)
  | ReuniteErrExtractTm !UniqueId !(SpecTm h UniqueId) !(GenQuant g) !ComplexResErr !(Graph g)
  -- ^ (id of type, reconstructed term, id of failed extract, graph)

deriving instance (Eq e, Eq (Node g), Eq (g (GenTy g)), Eq (h UniqueId (SpecTm h UniqueId))) => Eq (ReuniteErr e h g)
deriving instance (Show e, Show (Node g), Show (g (GenTy g)), Show (h UniqueId (SpecTm h UniqueId))) => Show (ReuniteErr e h g)

instance (Show e, Show (Node g), Show (g (GenTy g)), Show (h UniqueId (SpecTm h UniqueId)), Typeable e, Typeable h, Typeable g) => Exception (ReuniteErr e h g)

data ReuniteSuccess h g = ReuniteSuccess !UniqueId !(SpecTm h (GenQuant g)) !(GenQuant g) !(Graph g)

deriving instance (Eq (Node g), Eq (g (GenTy g)), Eq (h (GenQuant g) (SpecTm h (GenQuant g)))) => Eq (ReuniteSuccess h g)
deriving instance (Show (Node g), Show (g (GenTy g)), Show (h (GenQuant g) (SpecTm h (GenQuant g)))) => Show (ReuniteSuccess h g)

type ReuniteResult e h g = Either (ReuniteErr e h g) (ReuniteSuccess h g)

-- | Perform unification on a term in one go. -- NOTE It may be helpful to alias this function with the types filled in.
reuniteResult :: (Recursive t, Base t ~ f, Reunitable f h g, Alignable e g) => t -> (PreGraph g, ReuniteResult e h g)
reuniteResult = driveReuniteResult . reuniteTerm

quickReuniteResult ::
  (Recursive t, Base t ~ f, Reunitable f h g, Alignable e g, MonadThrow m,
  Show e, Show (Node g), Show (g (GenTy g)), Show (h UniqueId (SpecTm h UniqueId)), Typeable e, Typeable g, Typeable h) => t -> m (SpecTm h (GenQuant g), GenQuant g)
quickReuniteResult t =
  let r = snd (reuniteResult t)
  in case r of
    Left e -> throwM e
    Right (ReuniteSuccess _ tm u _) -> pure (tm, u)

driveReuniteResult :: (Bitraversable h, Alignable e g) => ReuniterM g (UniqueId, SpecTm h UniqueId) -> (PreGraph g, ReuniteResult e h g)
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
          case resolveGenVar bid graph of
            Left re -> Left (ReuniteErrExtractTy bid tm re graph)
            Right u ->
              case resolveTm tm graph of
                Left re -> Left (ReuniteErrExtractTm bid tm u re graph)
                Right tm' -> Right (ReuniteSuccess bid tm' u graph)
  in (pg, res)
