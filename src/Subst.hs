module Subst where

import Type

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
