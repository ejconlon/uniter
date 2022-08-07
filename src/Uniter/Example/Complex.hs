{-# LANGUAGE TemplateHaskell #-}

module Uniter.Example.Complex
  ( main
  ) where

import Control.Monad.Except (MonadError (..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Data.These (These (..))
import Uniter (Alignable (..), UnalignableErr (..), addNode, constrainEq, freshVar)
import Uniter.Example.Simple (M)
import Uniter.FreeEnv (FreeEnvMissingErr (..))
import qualified Uniter.FreeEnv as UF
import Uniter.Reunitable.Class (Reunion (..), Reunitable (..))

main :: IO ()
main = pure ()

data Ty =
    TyInt
  | TyFun Ty Ty
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Ty

data Exp =
    ExpInt !Int
  | ExpAdd Exp Exp
  | ExpIfZero Exp Exp Exp
  | ExpVar !Text
  | ExpApp Exp Exp
  | ExpAbs !Text !Exp
  | ExpLet !Text !Exp !Exp
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Exp

data AnnExp a =
    AnnExpInt !Int
  | AnnExpAdd (AnnExp a) (AnnExp a)
  | AnnExpIfZero (AnnExp a) (AnnExp a) (AnnExp a)
  | AnnExpVar !Text
  | AnnExpApp (AnnExp a) (AnnExp a)
  | AnnExpAbs !Text !a (AnnExp a)
  | AnnExpLet !Text !a (AnnExp a) (AnnExp a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''AnnExp

instance Alignable UnalignableErr TyF where
  align x y =
    case (x, y) of
      (TyIntF, TyIntF) -> Right TyIntF
      (TyFunF xa xb, TyFunF ya yb) -> Right (TyFunF (These xa ya) (These xb yb))
      _ -> Left UnalignableErr

instance Reunitable ExpF TyF AnnExp M where
  reunite = \case
    ExpIntF c -> do
      x <- addNode TyIntF
      pure (Reunion x (AnnExpInt c))
    ExpAddF mi mj -> do
      Reunion i hi <- mi
      Reunion j hj <- mj
      x <- addNode TyIntF
      _ <- constrainEq x i
      _ <- constrainEq x j
      pure (Reunion x (AnnExpAdd hi hj))
    ExpIfZeroF mi mj mk -> do
      Reunion i hi <- mi
      Reunion j hj <- mj
      Reunion k hk <- mk
      x <- addNode TyIntF
      y <- freshVar
      _ <- constrainEq x i
      _ <- constrainEq y j
      _ <- constrainEq y k
      pure (Reunion y (AnnExpIfZero hi hj hk))
    ExpVarF n -> do
      mv <- UF.lookupM n
      case mv of
        Nothing -> throwError (FreeEnvMissingErr n)
        Just v -> pure (Reunion v (AnnExpVar n))
    ExpAppF mi mj -> do
      Reunion i hi <- mi
      Reunion j hj <- mj
      x <- freshVar
      y <- addNode (TyFunF j x)
      _ <- constrainEq y i
      pure (Reunion x (AnnExpApp hi hj))
    ExpAbsF n mi -> do
      x <- freshVar
      Reunion y hy <- UF.insertM n x mi
      pure (Reunion y (AnnExpAbs n y hy))
    ExpLetF n mi mj -> do
      Reunion i hi <- mi
      Reunion y hy <- UF.insertM n i mj
      pure (Reunion y (AnnExpLet n i hi hy))

-- data ExpUniteErr = UniteErr UnalignableErr TyF

-- newtype N a = N { unN :: ReaderT (FreeEnv Text) (Except ExpUniteErr) a }
--   deriving newtype (Functor, Applicative, Monad, MonadReader (FreeEnv Text), MonadError ExpUnitErr)

-- expTy :: Exp -> M Ty
-- expTy = undefined

-- annExp :: Exp -> M AnnExp
-- annExp = undefined
