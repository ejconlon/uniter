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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Uniter.Align (Alignable)
import Uniter.Core (Node, TmVar, UniqueId, bareQuant)
import Uniter.Graph (Graph, SimpleResErr, resolveVar)
import Uniter.PreGraph (PreGraph (..))
import Uniter.Process (ProcessErr, embedReuniterM, extract, newProcessState, runProcessM)
import Uniter.Reunitable.Monad (ReuniterM, newReuniterEnv, newReuniterState, preGraph, runReuniterM)
import Uniter.Unitable.Class (Unitable, uniteTerm)

data UniteErr e g =
    UniteErrProcess !(ProcessErr e g)
  | UniteErrExtract !UniqueId !SimpleResErr !(Graph g)

deriving instance (Eq e, Eq (Node g)) => Eq (UniteErr e g)
deriving instance (Show e, Show (Node g)) => Show (UniteErr e g)

instance (Show e, Show (Node g), Typeable e, Typeable g) => Exception (UniteErr e g)

data UniteSuccess g u = UniteSuccess !UniqueId !u !(Graph g)
  deriving stock (Functor, Foldable, Traversable)

deriving instance (Eq (Node g), Eq u) => Eq (UniteSuccess g u)
deriving instance (Show (Node g), Show u) => Show (UniteSuccess g u)

type UniteResult e g u = Either (UniteErr e g) (UniteSuccess g u)

-- | Perform unification on a term in one go. NOTE It may be helpful to alias this function with the types filled in.
uniteResult :: (Recursive t, Base t ~ f, Unitable f g, Recursive u, Corecursive u, Base u ~ g, Alignable e g) => Map TmVar u -> t -> (PreGraph g, UniteResult e g u)
uniteResult fm = driveUniteResult fm . uniteTerm

quickUniteResult ::
  (Recursive t, Base t ~ f, Unitable f g, Recursive u, Corecursive u, Base u ~ g, Alignable e g, MonadThrow m,
  Show e, Show (Node g), Typeable e, Typeable g)
  => Map TmVar u -> t -> m u
quickUniteResult fm t =
  let r = snd (uniteResult fm t)
  in case r of
    Left e -> throwM e
    Right (UniteSuccess _ u _) -> pure u

driveUniteResult :: (Recursive u, Corecursive u, Base u ~ g, Alignable e g) => Map TmVar u -> ReuniterM g UniqueId -> (PreGraph g, UniteResult e g u)
driveUniteResult fm act =
  let fm' = fmap bareQuant fm
      uniq = toEnum 0
      pg = fromRight (error "impossible") (fst (runReuniterM (act *> preGraph) (newReuniterEnv fm') (newReuniterState uniq)))
      (ea, _) = flip runProcessM (newProcessState uniq) $ do
        bid <- embedReuniterM fm' act
        (_, graph) <- extract
        pure (bid, graph)
      res = case ea of
        Left pe -> Left (UniteErrProcess pe)
        Right (bid, graph) ->
          case resolveVar bid graph of
            Left re -> Left (UniteErrExtract bid re graph)
            Right u -> Right (UniteSuccess bid u graph)
  in (pg, res)
