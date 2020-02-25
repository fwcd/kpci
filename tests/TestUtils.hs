module TestUtils where

import Data.List (nub)

import Subst
import Type
import Vars

-- Fetches the maximum recursion depth of a given term.
depth :: Term -> Int
depth (Var _) = 0
depth (Comb _ ts) = 1 + (foldr max 0 $ map depth ts)

-- Tests whether two lists do not share elements.
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = foldr (\y -> ((not $ elem y xs) &&)) True

-- Fetches the number of distinct variables in a given term.
distinctVarCount :: Term -> Int
distinctVarCount = length . nub . allVars
