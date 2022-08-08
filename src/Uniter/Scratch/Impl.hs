{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Uniter.Scratch.Impl where
import Data.Kind (Type)
import GHC.Generics (Generic)
import Uniter.Scratch.Types (UniqId, PolyTy (..), TyVar (..), TyName, ForAll (..), PeelTy (..), noForAll, TmVar (..), mkPolyTy, wrapPolyTy)
import Control.Exception (Exception)
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State.Strict (State, MonadState (..), runState)
import Uniter.Scratch.Lang (XPolyTy, YTm (..), XPeelTy, XTyF (..))
import Data.Sequence (Seq)


data Env = Env
  -- { envVars :: !(Map VarTyName (SigmaType (Ty )))
  {
  } deriving stock (Eq, Ord, Show, Generic)

data St = St
  { stUniq :: !UniqId
  } deriving stock (Eq, Ord, Show, Generic)

data Err = Err1 | Err2
  deriving stock (Eq, Ord, Show)

instance Exception Err

newtype M (a :: Type) =
  M { unM :: ReaderT Env (ExceptT Err (State St)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState St, MonadError Err)

runM :: M a -> Env -> St -> (Either Err a, St)
runM m env = runState (runExceptT (runReaderT (unM m) env))

nextUniqId :: M UniqId
nextUniqId = undefined

lookupVar :: TyName -> M XPolyTy
lookupVar = undefined

-- inferPeel :: f (TyVar, h TyVar) -> M (TyVar, YTm)
-- inferPeel = undefined

-- prenex :: XPolyTy a ->

repeatTyLam :: Seq TyVar -> YTm TmVar -> YTm TmVar
repeatTyLam = undefined

repeatTyApp :: YTm TmVar -> Seq TyVar -> YTm TmVar
repeatTyApp = undefined

prenexConvertPoly :: XPolyTy -> M (ForAll XPeelTy, YTm TmVar)
prenexConvertPoly = \case
  PolyTyForAll (ForAll as pt) -> do
    (ForAll bs pt', f) <- prenexConvertPeel pt
    x <- fmap TmVarUniq nextUniqId
    let cs = as <> bs
        h = ForAll cs pt'
        g = YTmLam x (mkPolyTy cs pt') (repeatTyLam as (YTmApp f (repeatTyApp (YTmVar x) as)))
    pure (h, g)
  PolyTyPeel pt -> prenexConvertPeel pt

prenexConvertPeel :: XPeelTy -> M (ForAll XPeelTy, YTm TmVar)
prenexConvertPeel p@(PeelTy xtm) =
  case xtm of
    XTyFunF s1 s2 -> do
      (ForAll as pt', f) <- prenexConvertPoly s2
      x <- fmap TmVarUniq nextUniqId
      y <- fmap TmVarUniq nextUniqId
      let h = ForAll as (PeelTy (XTyFunF s1 (PolyTyPeel pt')))
          g = YTmLam x (wrapPolyTy h) (YTmLam y s1 (YTmApp f (repeatTyLam as (YTmApp (repeatTyApp (YTmVar x) as) (YTmVar y)))))
      pure (h, g)
    _ -> do
      x <- fmap TmVarUniq nextUniqId
      let q = noForAll p
          w = wrapPolyTy q
      pure (q, YTmLam x w (YTmVar x))

dsk :: XPolyTy -> XPolyTy -> m (Maybe (YTm TmVar))
dsk = undefined

dskStar :: XPolyTy -> XPeelTy -> m (Maybe (YTm TmVar))
dskStar = undefined
