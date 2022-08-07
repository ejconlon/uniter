{-# LANGUAGE TemplateHaskell #-}

module Uniter.Example.Complex
  ( main
  ) where

import Control.Monad.Except (MonadError (..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Data.These (These (..))
import Uniter (Alignable (..), UnalignableErr (..), Unitable (..), addNode, constrainEq, freshVar)
import Uniter.Example.Simple (M)
import Uniter.FreeEnv (FreeEnvMissingErr (..))
import qualified Uniter.FreeEnv as UF

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

data AnnExp =
    AnnExpInt !Int
  | AnnExpAdd AnnExp AnnExp
  | AnnExpIfZero AnnExp AnnExp AnnExp
  | AnnExpVar !Text
  | AnnExpApp AnnExp AnnExp
  | AnnExpAbs !Text !Ty AnnExp
  | AnnExpLet !Text !Ty AnnExp AnnExp
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''AnnExp

instance Alignable UnalignableErr TyF where
  align x y =
    case (x, y) of
      (TyIntF, TyIntF) -> Right TyIntF
      (TyFunF xa xb, TyFunF ya yb) -> Right (TyFunF (These xa ya) (These xb yb))
      _ -> Left UnalignableErr

instance Unitable ExpF TyF M where
  unite = \case
    ExpIntF _ -> addNode TyIntF
    ExpAddF mi mj -> do
      x <- addNode TyIntF
      _ <- mi >>= constrainEq x
      _ <- mj >>= constrainEq x
      pure x
    ExpIfZeroF mi mj mk -> do
      x <- addNode TyIntF
      y <- freshVar
      _ <- mi >>= constrainEq x
      _ <- mj >>= constrainEq y
      _ <- mk >>= constrainEq y
      pure y
    ExpVarF n -> do
      b <- UF.lookupM n
      maybe (throwError (FreeEnvMissingErr n)) pure b
    ExpAppF mi mj -> do
      i <- mi
      j <- mj
      x <- freshVar
      y <- addNode (TyFunF j x)
      _ <- constrainEq y i
      pure x
    ExpAbsF n mi -> do
      x <- freshVar
      UF.insertM n x mi
    ExpLetF n mi mj -> do
      i <- mi
      UF.insertM n i mj

-- data ExpUniteErr = UniteErr UnalignableErr TyF

-- newtype N a = N { unN :: ReaderT (FreeEnv Text) (Except ExpUniteErr) a }
--   deriving newtype (Functor, Applicative, Monad, MonadReader (FreeEnv Text), MonadError ExpUnitErr)

-- expTy :: Exp -> M Ty
-- expTy = undefined

-- annExp :: Exp -> N AnnExp
-- annExp = undefined
