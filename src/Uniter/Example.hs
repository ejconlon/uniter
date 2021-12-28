{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Uniter.Example where

import Control.Monad.Except (Except, MonadError, runExcept, ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict (State, MonadState (..), gets, modify', runState, StateT (runStateT))
import Data.Foldable (foldMap')
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Overeasy.Expressions.Free (Free, pattern FreeEmbed, pattern FreePure)
import Uniter.Exp (Exp (..), Ty, TyF (..))
import Control.Monad.Logic (LogicT)
import Control.Applicative (Alternative (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Void (Void)

data Void1 a

data Pair a b = Pair !a !b
  deriving stock (Eq, Show)

data Aligned f a b =
    AlignedVars !a !b
  | AlignedStructA !a !(f b)
  | AlignedStructB !(f a) !b
  deriving stock (Eq, Show)

alignedPair :: Pair a b -> Aligned f a b
alignedPair (Pair a b) = AlignedVars a b

newtype Embed f a = Embed { unEmbed :: f (Free f a) }
deriving stock instance Eq (f (Free f a)) => Eq (Embed f a)
deriving stock instance Show (f (Free f a)) => Show (Embed f a)

class Alignable m f where
  align :: f a -> f b -> LogicT m (Pair a b)

class AlignableAs m g f where
  alignAs :: f a -> f b -> LogicT m (Aligned g a b)

instance Alignable m f => AlignableAs m Void1 f where
  alignAs fa fb = fmap alignedPair (align fa fb)

instance Alignable m f => AlignableAs m (Embed f) (Free f) where
  alignAs fa fb =
    case fa of
      FreePure a ->
        case fb of
          FreePure b -> pure (AlignedVars a b)
          FreeEmbed tb -> pure (AlignedStructA a (Embed tb))
      FreeEmbed ta ->
        case fb of
          FreePure b -> pure (AlignedStructB (Embed ta) b)
          FreeEmbed tb -> alignAs (Embed ta) (Embed tb)

instance Alignable m f => AlignableAs m (Embed f) (Embed f) where
  alignAs (Embed ta) (Embed tb) = fixAlign (alignAs ta tb) where
    fixAlign :: LogicT m (Aligned Void1 (Free f a) (Free f b)) -> LogicT m (Aligned (Embed f) a b)
    fixAlign xs = xs >>= bindAlign
    bindAlign :: Aligned Void1 (Free f a) (Free f b) -> LogicT m (Aligned (Embed f) a b)
    bindAlign (AlignedVars fa fb) = alignAs fa fb

instance MonadFail m => Alignable m TyF where
  align ta tb =
    case ta of
      TyConstF ->
        case tb of
          TyConstF -> empty
          _ -> fail "const error"
      TyPairF fa1 fa2 ->
        case tb of
          TyConstF -> fail "pair error"
          TyPairF fb1 fb2 -> pure (Pair fa1 fb1) <|> pure (Pair fa2 fb2)

exampleLinear :: Exp
exampleLinear =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpSecond (ExpUseBind "v2")) (ExpFirst (ExpUseBind "v2"))) x4
      x4 = ExpUseBind "v3"
  in x1

exampleExponential :: Exp
exampleExponential =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpFirst (ExpUseBind "v2")) (ExpUseBind "v2")) x4
      x4 = ExpDefBind "v4" (ExpTuple (ExpSecond (ExpUseBind "v3")) (ExpTuple (ExpUseBind "v2") (ExpUseBind "v2"))) x5
      x5 = ExpUseBind "v4"
  in x1

newtype VarId = VarId { unVarId :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, Enum)

newtype Part f = Part { unPart :: Free f VarId }
deriving newtype instance Eq (f (Free f VarId)) => Eq (Part f)
deriving stock instance Show (f (Free f VarId)) => Show (Part f)

makePart :: (Recursive t, Base t ~ f) => t -> Part f
makePart = Part . go where
  go = FreeEmbed . fmap go . project

newtype BoundEnv f = BoundEnv { unBoundEnv :: Map VarId (Part f) }
deriving newtype instance Eq (f (Free f VarId)) => Eq (BoundEnv f)
deriving stock instance Show (f (Free f VarId)) => Show (BoundEnv f)

lookupWholeVar :: (Corecursive t, Base t ~ f, Traversable f) => BoundEnv f -> VarId -> Either VarId t
lookupWholeVar b@(BoundEnv m) v =
  case Map.lookup v m of
    Nothing -> Left v
    Just (Part w) -> lookupWholeFree b w

lookupWholePart :: (Corecursive t, Base t ~ f, Traversable f) => BoundEnv f -> Part f -> Either VarId t
lookupWholePart b = lookupWholeFree b . unPart

lookupWholeFree :: (Corecursive t, Base t ~ f, Traversable f) => BoundEnv f -> Free f VarId -> Either VarId t
lookupWholeFree b w =
  case w of
    FreePure v -> lookupWholeVar b v
    FreeEmbed ff -> fmap embed (traverse (lookupWholeFree b) ff)

newtype FreeEnv f = FreeEnv { unFreeEnv :: Map String (Part f) }
deriving newtype instance Eq (f (Free f VarId)) => Eq (FreeEnv f)
deriving stock instance Show (f (Free f VarId)) => Show (FreeEnv f)

data St f = St
  { stUnique :: !VarId
  , stFreeEnv :: !(FreeEnv f)
  , stBoundEnv :: !(BoundEnv f)
  }
deriving stock instance Eq (f (Free f VarId)) => Eq (St f)
deriving stock instance Show (f (Free f VarId)) => Show (St f)

data Error e =
    ErrorFail !String
  | ErrorMissingFree !String
  | ErrorEmbed !e
  deriving stock (Eq, Show)

newtype M e f a = M { unM :: StateT (St f) (Except (Error e)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (St f), MonadError (Error e))

runM :: M e f a -> St f -> Either (Error e) (a, St f)
runM m st = runExcept (runStateT (unM m) st)

class Substitutable f x | x -> f where
  subReplace :: BoundEnv f -> x -> x
  subBoundVars :: x -> [VarId]

instance (Functor f, Foldable f) => Substitutable f (Part f) where
  subReplace (BoundEnv m) = Part . go . unPart where
    go z =
      case z of
        FreePure v -> maybe z unPart (Map.lookup v m)
        FreeEmbed fx -> FreeEmbed (fmap go fx)
  subBoundVars = go . unPart where
    go = \case
      FreePure v -> [v]
      FreeEmbed fx -> foldMap go fx

instance (Functor f, Foldable f) => Substitutable f (BoundEnv f) where
  subReplace w@(BoundEnv s) (BoundEnv t) = BoundEnv (Map.union s (fmap (subReplace w) t))
  subBoundVars = foldMap subBoundVars . unBoundEnv

newVarM :: M e f (Part f)
newVarM = state $ \st ->
  let fresh = stUnique st
      st' = st { stUnique = succ fresh }
  in (Part (FreePure fresh), st')

replaceM :: (Functor f, Foldable f) => Part f -> M e f (Part f)
replaceM v = do
  m <- gets stBoundEnv
  pure (subReplace m v)

-- valUnifyM :: Val -> Val -> M Val
-- valUnifyM = go where
--   go v w =
--     case v of
--       ValConst ->
--         case w of
--           ValConst -> pure v
--           _ -> pure (ValErr "fail")

withFreeEnvM :: (FreeEnv f -> FreeEnv f) -> M e f a -> M e f a
withFreeEnvM f act = do
  freeEnv <- gets stFreeEnv
  modify' (\st -> st { stFreeEnv = f freeEnv })
  ret <- act
  modify' (\st -> st { stFreeEnv = freeEnv})
  pure ret

-- expM :: Exp -> M Void TyF (Part TyF)
-- expM = \case
--   ExpConst -> pure (Part (FreeEmbed TyConstF))
--   ExpUseBind n -> do
--     FreeEnv env <- gets stFreeEnv
--     case Map.lookup n env of
--       Just v -> pure (Part (FreePure v))
--       Nothing -> throwError (ErrorMissingFree n)
  -- ExpDefBind n x y -> do
  --   zx <- expM x
  --   withFreeEnvM (\(FreeEnv env) -> FreeEnv (Map.insert n zx env)) (expM y)
  -- ExpTuple x y -> do
  --   zx <- expM x
  --   zy <- expM y
  --   zx' <- replaceM zx
  --   pure (ValPair zx' zy)
  -- ExpFirst x -> do
  --   zx <- expM x
  --   v <- newVarM
  --   w <- newVarM
  --   valUnifyM (ValPair v w) zx
  --   replaceM v
  -- ExpSecond x -> do
  --   zx <- expM x
  --   v <- newVarM
  --   w <- newVarM
  --   valUnifyM (ValPair v w) zx
  --   replaceM w
