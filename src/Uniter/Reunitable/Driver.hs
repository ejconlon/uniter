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
import Data.Map.Strict (Map)
import Data.Typeable (Typeable)
import Uniter.Align (Alignable)
import Uniter.Core (BoundTy, GenQuant, Index, Node, SpecFinal, SpecInit, SrcQuant, TmVar, UniqueId)
import Uniter.Graph (ComplexResErr, Graph, resolveGenVar, resolveTm)
import Uniter.PreGraph (PreGraph (..))
import Uniter.Process (ProcessErr, embedReuniterM, extract, newProcessState, runProcessM)
import Uniter.Reunitable.Class (Reunitable, reuniteTerm)
import Uniter.Reunitable.Monad (ReuniterM, newReuniterEnv, newReuniterState, preGraph, runReuniterM)

data ReuniteErr e h g =
    ReuniteErrProcess !(ProcessErr e g)
  | ReuniteErrExtractTy !UniqueId !(SpecInit h g) !ComplexResErr !(Graph g)
  | ReuniteErrExtractTm !UniqueId !(SpecInit h g) !(GenQuant g) !ComplexResErr !(Graph g)

deriving instance (Eq e, Eq (Node g), Eq (g (BoundTy g Index)), Eq (h UniqueId (SpecInit h g))) => Eq (ReuniteErr e h g)
deriving instance (Show e, Show (Node g), Show (g (BoundTy g Index)), Show (h UniqueId (SpecInit h g))) => Show (ReuniteErr e h g)

instance (Show e, Show (Node g), Show (g (BoundTy g Index)), Show (h UniqueId (SpecInit h g)), Typeable e, Typeable h, Typeable g) => Exception (ReuniteErr e h g)

data ReuniteSuccess h g = ReuniteSuccess !UniqueId !(SpecFinal h g) !(GenQuant g) !(Graph g)

-- TODO
-- deriving instance (Eq (Node g), Eq (g (BoundTy g Index)), Eq (h (GenQuant g) (SpecFinal h g))) => Eq (ReuniteSuccess h g)
-- deriving instance (Show (Node g), Show (g (BoundTy g Index)), Show (h (GenQuant g) (SpecFinal h g))) => Show (ReuniteSuccess h g)

type ReuniteResult e h g = Either (ReuniteErr e h g) (ReuniteSuccess h g)

-- | Perform unification on a term in one go. -- NOTE It may be helpful to alias this function with the types filled in.
reuniteResult :: (Recursive t, Base t ~ f, Reunitable f h g, Alignable e g) => Map TmVar (SrcQuant g) -> t -> (PreGraph g, ReuniteResult e h g)
reuniteResult fm = driveReuniteResult fm . reuniteTerm

quickReuniteResult ::
  (Recursive t, Base t ~ f, Reunitable f h g, Alignable e g, MonadThrow m,
  Show e, Show (Node g), Show (g (BoundTy g Index)), Show (h UniqueId (SpecInit h g)), Typeable e, Typeable g, Typeable h)
  => Map TmVar (SrcQuant g) -> t -> m (SpecFinal h g, GenQuant g)
quickReuniteResult fm t =
  let r = snd (reuniteResult fm t)
  in case r of
    Left e -> throwM e
    Right (ReuniteSuccess _ tm u _) -> pure (tm, u)

driveReuniteResult :: (Bitraversable h, Alignable e g) => Map TmVar (SrcQuant g) -> ReuniterM g (UniqueId, SpecInit h g) -> (PreGraph g, ReuniteResult e h g)
driveReuniteResult fm act =
  let uniq = toEnum 0
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
                Right sq -> Right (ReuniteSuccess bid sq u graph)
  in (pg, res)
