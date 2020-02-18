module Subst where

import Data.List (intercalate)
import Pretty
import Type
import Vars

data Subst = Subst [(VarName, Term)]
  deriving Show

-- An empty substitution.
empty :: Subst
empty = Subst []

-- Creates a substitution that maps a single variable to a term.
single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

-- Applies a substitution to a term.
apply :: Subst -> Term -> Term
apply (Subst s) (Var v) = maybe (Var v) id $ lookup v s
apply subst (Comb name ts) = Comb name $ map (apply subst) ts

-- Restricts a substitution to the given variable names.
restrictTo :: [VarName] -> Subst -> Subst
restrictTo vs (Subst s) = Subst $ filter (flip elem vs . fst) s

-- Creates a substitution by first applying the right and then the left one.
compose :: Subst -> Subst -> Subst
compose (Subst s2) (Subst s1) = Subst $ [(v, apply (Subst s2) t) | (v, t) <- s1]
                                     ++ [(v, t)                  | (v, t) <- s2, not $ elem v $ map fst s1]

instance Pretty Subst where
  pretty (Subst s) = "{" ++ intercalate ", " (map prettyMapping s) ++ "}"
    where prettyMapping (v, t) = v ++ " -> " ++ pretty t

instance Vars Subst where
  allVars (Subst s) = (map fst s) ++ ((map snd s) >>= allVars)
