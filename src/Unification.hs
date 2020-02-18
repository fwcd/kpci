module Unification (ds, unify) where

import Data.Maybe (listToMaybe, maybeToList)
import Subst
import Type
import Vars

-- Computes the disagreement set.
ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 = case (t1, t2) of
                (Var x, Var y) | x /= y && x /= "_" && y /= "_" -> Just (t1, t2)
                               | otherwise                      -> Nothing
                (Var _, Comb _ _) -> Just (t1, t2)
                (Comb _ _, Var _) -> Just (t1, t2)
                (Comb f ts1, Comb g ts2) | f /= g || n /= m -> Just (t1, t2)
                                         | otherwise        -> listToMaybe $ (zip ts1 ts2) >>= (maybeToList . uncurry ds)
                    where n = length ts1
                          m = length ts2

-- Unifies two terms.
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = case ds t1 t2 of
    Just (d1, d2) -> do
        s1 <- case (d1, d2) of
                (Var x, t) | not $ elem x $ allVars t -> Just $ single x d2
                (t, Var x) | not $ elem x $ allVars t -> Just $ single x d1
                _                                     -> Nothing
        s2 <- unify (apply s1 t1) (apply s1 t2)
        Just $ compose s1 s2
    Nothing       -> Just empty
