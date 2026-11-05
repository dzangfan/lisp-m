{-# LANGUAGE LambdaCase #-}

module Lisp where

import Control.Monad (when, liftM2)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow ((>>>), first)
import Data.Maybe (maybe)
import Data.Function ((&))
import Data.Bool (bool)

type Name = String

data SExp a = SAt a | SPr (SExp a) (SExp a)
  deriving Eq
data MExp a
  = MLit (SExp a) | MVar Name
  | MApp Name [MExp a]
  | MCnd [(MExp a, MExp a)]
  | MAtom (MExp a)
  | MEq   (MExp a) (MExp a)
  | MCons (MExp a) (MExp a)
  | MCar  (MExp a)
  | MCdr  (MExp a)
  deriving Eq

type MDef a = (Name, ([Name], MExp a))

type Env a = [(Name, SExp a)]
type Def a = [MDef a]
type Partial = MonadError String
class AtBool a where
  inBool  :: Bool -> a
  outBool :: Partial m => a -> m Bool
  outBoolSExp :: Partial m => SExp a -> m Bool
  outBoolSExp (SAt a) = outBool a
  outBoolSExp _       = throwError "S-exp is not a boolean"

evalMExp :: (Partial m, MonadReader (Env a, Def a) m, AtBool a, Eq a)
         => MExp a -> m (SExp a)
evalMExp m = case m of
  MLit s -> return s
  MVar x -> asks (fst >>> lookup x)
    >>= maybe (throwError ("Undefined variable: " ++ x)) return
  MApp f ms -> do
    (xs, m) <-
      asks (snd >>> lookup f)
      >>= maybe (throwError ("Undefined function: " ++ f)) return
    when (length xs /= length ms)
      (throwError (f ++ " expected "                ++ show (length xs)
                     ++ " arguments, but received " ++ show (length ms)))
    vs <- mapM evalMExp ms
    local (first (zip xs vs ++)) (evalMExp m)
  MCnd cvs -> cvs &
    foldr (\(c, v) m -> evalMExp c >>= outBoolSExp >>= bool m (evalMExp v))
          (throwError "Non-exhaustive condition")
  MAtom m -> evalMExp m
    & fmap (SAt . inBool . (\case (SAt _) -> True; _ -> False))
  MEq m n -> do
    s <- evalMExp m; t <- evalMExp n
    case (s, t) of
      (SAt a, SAt b) -> return (SAt (inBool (a == b)))
      _ -> throwError "Invoked eq with non-atoms"
  MCons m n -> liftM2 SPr (evalMExp m) (evalMExp n)
  MCar m -> evalMExp m >>= \case
    SPr a b -> return a
    _ -> throwError "Invoked car with atoms"
  MCdr m -> evalMExp m >>= \case
    SPr a b -> return b
    _ -> throwError "Invoked cdr with atoms"

