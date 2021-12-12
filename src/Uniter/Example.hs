{-# LANGUAGE TemplateHaskell #-}

module Uniter.Example where

import Control.Monad.Except (Except, MonadError, runExcept, ExceptT, runExceptT)
import Control.Monad.State.Strict (State, MonadState (..), gets, modify', runState, StateT (runStateT))
import Data.Foldable (foldMap')
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Overeasy.Expressions.Free (Free)

data Exp =
    ExpConst
  | ExpUseBind !String
  | ExpDefBind !String Exp Exp
  | ExpTuple Exp Exp
  | ExpFirst Exp
  | ExpSecond Exp
  deriving stock (Eq, Show)

makeBaseFunctor ''Exp

data Ty =
    TyPair Ty Ty
  | TyConst
  deriving stock (Eq, Show)

makeBaseFunctor ''Ty

data Aligned f a b =
    AlignedVars !a !b
  | AlignedStructA !a !(f b)
  | AlignedStructB !(f a) !b
  deriving stock (Eq, Show)

alignFreeTy :: Free TyF a -> Free TyF b -> Maybe (Seq (Aligned (Free TyF) a b))
alignFreeTy fa fb =
  case fa of
    _ -> undefined

alignTyF :: TyF (Free TyF a) -> TyF (Free TyF b) -> Maybe (Seq (Aligned (Free TyF) a b))
alignTyF ta tb =
  case ta of
    TyConstF ->
      case tb of
        TyConstF -> Just Empty
        _ -> Nothing
    TyPairF fa1 fa2 ->
      case tb of
        TyConstF -> Nothing
        TyPairF fb1 fb2 -> (<>) <$> alignFreeTy fa1 fb1 <*> alignFreeTy fa2 fb2

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

newtype TyRel = VM { unVM :: Map VarId (Free TyF VarId) }
  -- deriving stock (Show)
  -- deriving newtype (Eq)

-- emptyVM :: VM
-- emptyVM = VM Map.empty

-- lookupVM :: VarId -> VM -> Maybe Val
-- lookupVM k (VM m) = Map.lookup k m

-- singletonVM :: VarId -> Val -> VM
-- singletonVM k v = VM (Map.singleton k v)

-- unionVM :: VM -> VM -> VM
-- unionVM (VM m1) (VM m2) = VM (Map.union m1 m2)

newtype TyEnv = TyEnv { unTyEnv :: Map String Ty }
  deriving stock (Show)
  deriving newtype (Eq)

data St = St
  { stUniq :: !VarId
  , stTyEnv :: !TyEnv
  , stTyRel :: !TyRel
  } -- deriving stock (Eq, Show)

data Err = ErrFail !String
  deriving stock (Eq, Show)

newtype M a = M { unM :: ExceptT Err (State St) a }
  deriving newtype (Functor, Applicative, Monad, MonadState St, MonadError Err)

runM :: M a -> St -> (Either Err a, St)
runM = runState . runExceptT . unM

-- class Substitutable x where
--   subReplace :: VM -> x -> x
--   subFreeVars :: x -> Set VarId

-- instance Substitutable Val where
--   subReplace (VM s) = go where
--     go = \case
--       ValPair v w -> ValPair (go v) (go w)
--       v@(ValVar i) -> fromMaybe v (Map.lookup i s)
--       v -> v
--   subFreeVars = \case
--     ValPair v w -> Set.union (subFreeVars v) (subFreeVars w)
--     ValVar i -> Set.singleton i
--     _ -> Set.empty

-- instance Substitutable VM where
--   subReplace m@(VM s) (VM t) = VM (Map.union s (Map.map (subReplace m) t))
--   subFreeVars = foldMap' subFreeVars . unVM

-- newVarM :: M Val
-- newVarM = state $ \st ->
--   let fresh = stUniq st
--       st' = st { stUniq = succ fresh }
--   in (ValVar fresh, st')

-- replaceM :: Val -> M Val
-- replaceM v = do
--   m <- gets stVM
--   pure (subReplace m v)

-- valUnifyM :: Val -> Val -> M Val
-- valUnifyM = go where
--   go v w =
--     case v of
--       ValConst ->
--         case w of
--           ValConst -> pure v
--           _ -> pure (ValErr "fail")

-- withEnvM :: (Env -> Env) -> M a -> M a
-- withEnvM f act = do
--   env <- gets stEnv
--   modify' (\st -> st { stEnv = f env })
--   ret <- act
--   modify' (\st -> st { stEnv = env})
--   pure ret

-- expM :: Exp -> M Val
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
