{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simple example following "Efficient Functional Unification and Substitution"
-- by Dijkstra, Middelkoop, and Swierstra.
module Uniter.Example.Simple
  ( Exp (..)
  , ExpF (..)
  , Ty (..)
  , TyF (..)
  , M
  , runM
  , exampleLinear
  , exampleExponential
  , processVerbose
  , main
  ) where

import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import Data.These (These (..))
import Text.Pretty.Simple (pPrint)
import Uniter (Alignable (..), ExtractErr (..), UnalignableErr (..), Unitable (..), UniteResult (..), addNode,
               constrainEq, freshVar, uniteResult)
import Uniter.FreeEnv (FreeEnv, FreeEnvMissingErr (..))
import qualified Uniter.FreeEnv as UF
import Uniter.Render (writeGraphDot, writePreGraphDot)

-- | A simple expression language with constants, vars, lets, tuples, and projections.
data Exp =
    ExpConst
  | ExpUseBind !Text
  | ExpDefBind !Text Exp Exp
  | ExpTuple Exp Exp
  | ExpFirst Exp
  | ExpSecond Exp
  deriving stock (Eq, Show)

-- TH can build us an expression functor ExpF to factor our the recursion.
makeBaseFunctor ''Exp

deriving stock instance Eq a => Eq (ExpF a)
deriving stock instance Show a => Show (ExpF a)

-- | Our expressions are either constant type or pairs of types.
data Ty =
    TyPair Ty Ty
  | TyConst
  deriving stock (Eq, Show)

-- Builds a type functor TyF
makeBaseFunctor ''Ty

deriving stock instance Eq a => Eq (TyF a)
deriving stock instance Show a => Show (TyF a)

instance Alignable UnalignableErr TyF where
  -- Align our type functor in the natural way
  align TyConstF TyConstF = Right TyConstF
  align (TyPairF a b) (TyPairF c d) = Right (TyPairF (These a c) (These b d))
  align _ _ = Left UnalignableErr

-- | The effects we'll use for unification - reader for maintaining a var env, IO for debug output
newtype M a = M { unM :: ReaderT (FreeEnv Text) (ExceptT (FreeEnvMissingErr Text) IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (FreeEnv Text), MonadError (FreeEnvMissingErr Text), MonadIO, MonadThrow)

runM :: M a -> IO a
runM m = runExceptT (runReaderT (unM m) UF.empty) >>= either throwIO pure

instance Unitable TyF ExpF M where
  -- Inspect our expression functor and perform unification
  unite = \case
    ExpConstF ->
      -- constants have constant type
      addNode TyConstF
    ExpUseBindF n -> do
      -- vars require we lookup the binding, throwing if it's not there
      b <- UF.lookupM n
      maybe (throwError (FreeEnvMissingErr n)) pure b
    ExpDefBindF n mx my -> do
      -- first get the type of the argument of the let
      x <- mx
      -- then bind the var, and return the type of the body with it bound
      UF.insertM n x my
    ExpTupleF mx my -> do
      -- find the type of both arguments
      x <- mx
      y <- my
      -- and tuple them together!
      addNode (TyPairF x y)
    ExpFirstF mx -> do
      -- find the type of the argument
      x <- mx
      -- and ensure it unifies with a tuple type
      v <- freshVar
      w <- freshVar
      y <- addNode (TyPairF v w)
      _ <- constrainEq x y
      -- fst returns the type of the first element of the pair
      pure v
    ExpSecondF mx -> do
      -- just like first, find the type of the argument
      x <- mx
      -- and again ensure it unifies with a tuple type
      v <- freshVar
      w <- freshVar
      y <- addNode (TyPairF v w)
      _ <- constrainEq x y
      -- BUT return something different here:
      -- snd returns the type of the second element of the pair
      pure w

-- | A small example of type (C, C)
exampleLinear :: Exp
exampleLinear =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpSecond (ExpUseBind "v2")) (ExpFirst (ExpUseBind "v2"))) x4
      x4 = ExpUseBind "v3"
  in x1

-- | An example that can easily grow larger: ((C, C), ((C, C), (C, C)))
exampleExponential :: Exp
exampleExponential =
  let x1 = ExpDefBind "v1" ExpConst x2
      x2 = ExpDefBind "v2" (ExpTuple (ExpUseBind "v1") (ExpUseBind "v1")) x3
      x3 = ExpDefBind "v3" (ExpTuple (ExpFirst (ExpUseBind "v2")) (ExpUseBind "v2")) x4
      x4 = ExpDefBind "v4" (ExpTuple (ExpSecond (ExpUseBind "v3")) (ExpTuple (ExpUseBind "v2") (ExpUseBind "v2"))) x5
      x5 = ExpUseBind "v4"
  in x1

-- | A complete example of how to infer the type of an expression
-- with unification through 'Unitable' and 'Alignable'.
processVerbose :: String -> Exp -> IO Ty
processVerbose name expr = runM go where
  go = do
    liftIO $ putStrLn ("*** Processing example: " ++ show name)
    liftIO $ putStrLn "--- Expression:"
    pPrint expr
    (pg, res) <- uniteResult expr
    liftIO $ writePreGraphDot ("dot/" ++ name ++ "-initial.dot") pg
    case res of
      UniteResultProcessErr pe -> do
        liftIO $ putStrLn "--- Process failure"
        throwM pe
      UniteResultExtractErr bid xid rs g -> do
        liftIO $ putStrLn ("--- Extract failure: " ++ show xid)
        goVerbose bid rs g
        throwM (ExtractErr bid xid)
      UniteResultSuccess bid ty rs g -> do
        liftIO $ putStrLn "--- Success"
        goVerbose bid rs g
        liftIO $ putStrLn "--- Final type: "
        pPrint ty
        pure ty
  goVerbose bid rs g = liftIO $ do
    writeGraphDot ("dot/" ++ name ++ "-processed.dot") g
    putStrLn ("--- Expr id: " ++ show bid)
    putStrLn "--- Rebind map:"
    pPrint rs
    putStrLn "--- Final graph:"
    pPrint g

main :: IO ()
main = do
  void $ processVerbose "linear" exampleLinear
  void $ processVerbose "exponential" exampleExponential
