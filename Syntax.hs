{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Control.Monad.Except (throwError)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as P

import Lisp

-- Atoms are capitalized symbols; T and NIL serve as booleans
instance AtBool String where
  inBool True  = "T"
  inBool False = "NIL"
  outBool "T"   = return True
  outBool "NIL" = return False
  outBool a     = throwError ("Atom is not a boolean: " ++ a)

-- Lexemes

sc :: Parser ()
sc = spaces

lexeme :: Parser a -> Parser a
lexeme p = p <* sc

symbol :: String -> Parser String
symbol = lexeme . try . string

parens, brackets, braces :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
braces   = between (symbol "{") (symbol "}")

ident :: Parser Name
ident = lexeme (many1 lower) <?> "identifier"

atom :: Parser String
atom = lexeme ((:) <$> upper <*> many (upper <|> digit)) <?> "atom"

-- S-expressions: A | () | (s ...) | (s ... . s)

nil :: SExp String
nil = SAt "NIL"

sexp :: Parser (SExp String)
sexp = SAt <$> atom <|> parens body <?> "S-expression"
  where
    body = do
      xs  <- many sexp
      end <- if null xs then return nil
             else option nil (symbol "." *> sexp)
      return (foldr SPr end xs)

-- Macro names are tagged with #; a macro call #m[...] is kept as an
-- application of the function named #m until it is expanded
macroName :: Parser Name
macroName = lexeme ((:) <$> char '#' <*> many1 lower) <?> "macro name"

-- M-expressions: x | f[m; ...] | #m[m; ...] | {m -> m; ...} | S-expression

mexp :: Parser (MExp String)
mexp = cond <|> (MLit <$> sexp) <|> macroCall <|> varOrApp <?> "M-expression"
  where
    macroCall = MApp <$> macroName <*> brackets (mexp `sepBy` symbol ";")
    varOrApp = do
      f <- ident
      option (MVar f) (brackets (mexp `sepBy` symbol ";") >>= builtin f)
    cond   = MCnd <$> braces (clause `sepBy1` symbol ";")
    clause = (,) <$> mexp <* symbol "->" <*> mexp

builtins :: [Name]
builtins = ["atom", "eq", "cons", "car", "cdr"]

builtin :: Name -> [MExp String] -> Parser (MExp String)
builtin f ms = either fail return (apply f ms)

-- An application of f, which may be a built-in
apply :: Name -> [MExp String] -> Either String (MExp String)
apply "atom" [m]    = Right (MAtom m)
apply "eq"   [m, n] = Right (MEq m n)
apply "cons" [m, n] = Right (MCons m n)
apply "car"  [m]    = Right (MCar m)
apply "cdr"  [m]    = Right (MCdr m)
apply f ms
  | f `elem` builtins = Left ("Wrong number of arguments for " ++ f)
  | otherwise         = Right (MApp f ms)

-- Definitions: f[x; ...] = m, and macros: #m[x; ...] = m

mdef :: Parser (MDef String)
mdef = do
  (f, xs) <- try header
  m <- mexp
  return (f, (xs, m))
  where
    header = (,) <$> (macroName <|> ident) <*> brackets (ident `sepBy` symbol ";") <* symbol "="

-- Programs: a sequence of definitions and expressions

program :: Parser [Either (MDef String) (MExp String)]
program = sc *> many (Left <$> mdef <|> Right <$> mexp) <* eof

parseProgram :: SourceName -> String
             -> Either ParseError [Either (MDef String) (MExp String)]
parseProgram = parse program

-- Pretty-printing: the output can be parsed back by program

prettySExp :: SExp String -> Doc
prettySExp (SAt a) = P.text a
prettySExp (SPr a b) = P.parens (P.sep (prettySExp a : rest b))
  where
    rest (SAt "NIL") = []
    rest (SAt x)     = [P.text "." <+> P.text x]
    rest (SPr x y)   = prettySExp x : rest y

prettyMExp :: MExp String -> Doc
prettyMExp m = case m of
  MLit s    -> prettySExp s
  MVar x    -> P.text x
  MApp f ms -> app f ms
  MCnd cs   -> P.braces (P.sep (P.punctuate P.semi (map clause cs)))
  MAtom n   -> app "atom" [n]
  MEq n o   -> app "eq" [n, o]
  MCons n o -> app "cons" [n, o]
  MCar n    -> app "car" [n]
  MCdr n    -> app "cdr" [n]
  where
    app f ms = P.text f <> P.brackets (P.sep (P.punctuate P.semi (map prettyMExp ms)))
    clause (c, e) = P.hang (prettyMExp c <+> P.text "->") 2 (prettyMExp e)

prettyMDef :: MDef String -> Doc
prettyMDef (f, (xs, m)) = header <+> P.equals <+> prettyMExp m
  where header = P.text f <> P.brackets (P.hsep (P.punctuate P.semi (map P.text xs)))

prettyProgram :: [Either (MDef String) (MExp String)] -> Doc
prettyProgram = P.vcat . map (either prettyMDef prettyMExp)

-- Read and Show via the parser and the pretty-printer

readsWith :: Parser a -> ReadS a
readsWith p s = either (const []) pure (parse ((,) <$> (sc *> p) <*> getInput) "" s)

instance Read (SExp String) where
  readsPrec _ = readsWith sexp

instance Read (MExp String) where
  readsPrec _ = readsWith mexp

instance Show (SExp String) where
  showsPrec _ = showString . P.render . prettySExp

instance Show (MExp String) where
  showsPrec _ = showString . P.render . prettyMExp
