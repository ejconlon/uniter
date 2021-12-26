{-# LANGUAGE UndecidableInstances #-}

module Uniter.Example where

import Control.Monad.Except (Except, MonadError, runExcept, ExceptT, runExceptT)
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

data Void1 a

data Aligned f a b =
    AlignedVars !a !b
  | AlignedStructA !a !(f b)
  | AlignedStructB !(f a) !b
  deriving stock (Eq, Show)

newtype Embed f a = Embed { unEmbed :: f (Free f a) }
deriving stock instance Eq (f (Free f a)) => Eq (Embed f a)
deriving stock instance Show (f (Free f a)) => Show (Embed f a)

class Alignable m g f where
  align :: f a -> f b -> LogicT m (Aligned g a b)

instance Alignable m Void1 f => Alignable m (Embed f) (Free f) where
  align fa fb =
    case fa of
      FreePure a ->
        case fb of
          FreePure b -> pure (AlignedVars a b)
          FreeEmbed tb -> pure (AlignedStructA a (Embed tb))
      FreeEmbed ta ->
        case fb of
          FreePure b -> pure (AlignedStructB (Embed ta) b)
          FreeEmbed tb -> align (Embed ta) (Embed tb)

instance Alignable m Void1 f => Alignable m (Embed f) (Embed f) where
  align (Embed ta) (Embed tb) = fixAlign (align ta tb) where
    fixAlign :: LogicT m (Aligned Void1 (Free f a) (Free f b)) -> LogicT m (Aligned (Embed f) a b)
    fixAlign xs = xs >>= bindAlign
    bindAlign :: Aligned Void1 (Free f a) (Free f b) -> LogicT m (Aligned (Embed f) a b)
    bindAlign (AlignedVars fa fb) = align fa fb

instance MonadFail m => Alignable m Void1 TyF where
  align ta tb =
    case ta of
      TyConstF ->
        case tb of
          TyConstF -> empty
          _ -> fail "const error"
      TyPairF fa1 fa2 ->
        case tb of
          TyConstF -> fail "pair error"
          TyPairF fb1 fb2 -> pure (AlignedVars fa1 fb1) <|> pure (AlignedVars fa2 fb2)

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

newtype TyRel f = TyRel { unTyRel :: Map VarId (Part f) }
deriving newtype instance Eq (f (Free f VarId)) => Eq (TyRel f)
deriving stock instance Show (f (Free f VarId)) => Show (TyRel f)

newtype TyEnv f = TyEnv { unTyEnv :: Map String (Part f) }
deriving newtype instance Eq (f (Free f VarId)) => Eq (TyEnv f)
deriving stock instance Show (f (Free f VarId)) => Show (TyEnv f)

data St f = St
  { stUniq :: !VarId
  , stTyEnv :: !(TyEnv f)
  , stTyRel :: !(TyRel f)
  }
deriving stock instance Eq (f (Free f VarId)) => Eq (St f)
deriving stock instance Show (f (Free f VarId)) => Show (St f)

newtype M e f a = M { unM :: ExceptT e (State (St f)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (St f), MonadError e)

runM :: M e f a -> St f -> (Either e a, St f)
runM = runState . runExceptT . unM

class Substitutable f x | x -> f where
  subReplace :: TyRel f -> x -> x
  subFreeVars :: x -> Set VarId

-- instance Substitutable f (Part f) where
--   subReplace (VM s) = go where
--     go = \case
--       ValPair v w -> ValPair (go v) (go w)
--       v@(ValVar i) -> fromMaybe v (Map.lookup i s)
--       v -> v
--   subFreeVars = \case
--     ValPair v w -> Set.union (subFreeVars v) (subFreeVars w)
--     ValVar i -> Set.singleton i
--     _ -> Set.empty

-- instance Substitutable f (TyRel f) where
--   subReplace m@(VM s) (VM t) = VM (Map.union s (Map.map (subReplace m) t))
--   subFreeVars = foldMap' subFreeVars . unVM

newVarM :: M e f VarId
newVarM = state $ \st ->
  let fresh = stUniq st
      st' = st { stUniq = succ fresh }
  in (fresh, st')

-- replaceM :: Part f -> M e f (Part f)
-- replaceM v = do
--   m <- gets stTyEnv
--   pure (subReplace m v)

-- valUnifyM :: Val -> Val -> M Val
-- valUnifyM = go where
--   go v w =
--     case v of
--       ValConst ->
--         case w of
--           ValConst -> pure v
--           _ -> pure (ValErr "fail")

-- withEnvM :: (Env f -> Env f) -> M f a -> M f a
-- withEnvM f act = do
--   env <- gets stEnv
--   modify' (\st -> st { stEnv = f env })
--   ret <- act
--   modify' (\st -> st { stEnv = env})
--   pure ret

-- expM :: Exp -> M TyF (Part TyF)
-- expM = \case
--   ExpConst -> pure ValConst
--   ExpUseBind n -> do
--     Env env <- gets stEnv
--     case Map.lookup n env of
--       Just v -> replaceM v
--       Nothing -> pure (ValErr ("not found: " ++ show n))
--   ExpDefBind n x y -> do
--     zx <- expM x
--     withEnvM (\(Env env) -> Env (Map.insert n zx env)) (expM y)
--   ExpTuple x y -> do
--     zx <- expM x
--     zy <- expM y
--     zx' <- replaceM zx
--     pure (ValPair zx' zy)
--   ExpFirst x -> do
--     zx <- expM x
--     v <- newVarM
--     w <- newVarM
--     valUnifyM (ValPair v w) zx
--     replaceM v
--   ExpSecond x -> do
--     zx <- expM x
--     v <- newVarM
--     w <- newVarM
--     valUnifyM (ValPair v w) zx
--     replaceM w
