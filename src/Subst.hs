module Subst where

data Subst = Subst [(VarName, Term)]

-- An empty substitution.
empty :: Subst
empty = Subst []

-- Creates a substitution that maps a single variable to a term.
single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]
