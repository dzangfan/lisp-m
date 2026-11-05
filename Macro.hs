{-# LANGUAGE LambdaCase #-}

-- Macros: #m[x; ...] = e defines a macro, and a call #m[a; ...] is
-- replaced by e with each x replaced by its a, before evaluation.

module Macro (isMacro, expand) where

import Control.Monad (when)

import Lisp
import Syntax (apply)

isMacro :: Name -> Bool
isMacro = (== "#") . take 1

-- Expansions nested deeper than this are taken to be endless recursion
limit :: Int
limit = 1000

-- Replace every macro call, outermost first, until none is left. The
-- definitions may include functions; only the macros among them are used.
expand :: Def String -> MExp String -> Either String (MExp String)
expand ds = go 0
  where
    go d = \case
      MApp f ms | isMacro f -> do
        (xs, body) <- maybe (Left ("Undefined macro: " ++ f)) Right (lookup f ds)
        when (length xs /= length ms)
          (Left (f ++ " expected "                ++ show (length xs)
                   ++ " arguments, but received " ++ show (length ms)))
        when (d >= limit)
          (Left ("Macro expansion too deep, " ++ f ++ " may be recursive"))
        subst (zip xs ms) body >>= go (d + 1)
      m -> descend (go d) m

-- Arguments replace parameters, also where a parameter names a function
subst :: [(Name, MExp String)] -> MExp String -> Either String (MExp String)
subst env = \case
  MVar x | Just m <- lookup x env -> Right m
  MApp f ms | Just m <- lookup f env -> case m of
    MVar g -> mapM (subst env) ms >>= apply g
    _ -> Left ("Parameter " ++ f ++ " is called as a function, but its argument is not a name")
  m -> descend (subst env) m

-- Apply f to each immediate subexpression
descend :: Monad m => (MExp a -> m (MExp a)) -> MExp a -> m (MExp a)
descend f = \case
  MLit s    -> pure (MLit s)
  MVar x    -> pure (MVar x)
  MApp g ms -> MApp g <$> mapM f ms
  MCnd cs   -> MCnd <$> mapM (\(c, e) -> (,) <$> f c <*> f e) cs
  MAtom m   -> MAtom <$> f m
  MEq m n   -> MEq <$> f m <*> f n
  MCons m n -> MCons <$> f m <*> f n
  MCar m    -> MCar <$> f m
  MCdr m    -> MCdr <$> f m
