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
import Data.Either (fromRight)
import Data.Functor.Foldable (Base, Corecursive, Recursive)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Uniter.Align (Alignable)
import Uniter.Core (Node, SynVar (..))
import Uniter.Graph (Graph, resolveVar)
import Uniter.PreGraph (PreGraph (..))
import Uniter.Process (ProcessErr, RebindMap, embedReuniterM, extract, newProcessState, runProcessM)
import Uniter.Reunitable.Monad (ReuniterM, newReuniterEnv, newReuniterState, preGraph, runReuniterM)
import Uniter.Unitable.Class (Unitable, uniteTerm)

data UniteErr e g =
    UniteErrProcess !(ProcessErr e)
  | UniteErrExtract !SynVar !SynVar !RebindMap !(Graph g)

deriving instance (Eq e, Eq (Node g)) => Eq (UniteErr e g)
deriving instance (Show e, Show (Node g)) => Show (UniteErr e g)

instance (Show e, Show (Node g), Typeable e, Typeable g) => Exception (UniteErr e g)

data UniteSuccess g u = UniteSuccess !SynVar !u !RebindMap !(Graph g)
  deriving stock (Functor, Foldable, Traversable)

deriving instance (Eq (Node g), Eq u) => Eq (UniteSuccess g u)
deriving instance (Show (Node g), Show u) => Show (UniteSuccess g u)

type UniteResult e g u = Either (UniteErr e g) (UniteSuccess g u)

-- | Perform unification on a term in one go. -- NOTE It may be helpful to alias this function with the types filled in.
uniteResult :: (Recursive t, Base t ~ f, Unitable f g, Corecursive u, Base u ~ g, Alignable e g) => t -> (PreGraph g, UniteResult e g u)
uniteResult = driveUniteResult . uniteTerm

quickUniteResult :: (Recursive t, Base t ~ f, Unitable f g, Corecursive u, Base u ~ g, Alignable e g, MonadThrow m, Show e, Show (Node g), Typeable e, Typeable g) => t -> m u
quickUniteResult t =
  let r = snd (uniteResult t)
  in case r of
    Left e -> throwM e
    Right (UniteSuccess _ u _ _) -> pure u

driveUniteResult :: (Corecursive u, Base u ~ g, Alignable e g) => ReuniterM g SynVar -> (PreGraph g, UniteResult e g u)
driveUniteResult act =
  let fm = Map.empty
      uniq = toEnum 0
      pg = fromRight (error "bad program") (fst (runReuniterM (act *> preGraph) (newReuniterEnv fm) (newReuniterState uniq)))
      (ea, _) = flip runProcessM (newProcessState uniq) $ do
        bid <- embedReuniterM fm act
        (rebinds, graph) <- extract
        pure (bid, rebinds, graph)
      res = case ea of
        Left pe -> Left (UniteErrProcess pe)
        Right (bid, rebinds, graph) ->
          case resolveVar bid graph of
            Left xid -> Left (UniteErrExtract bid xid rebinds graph)
            Right u -> Right (UniteSuccess bid u rebinds graph)
  in (pg, res)
