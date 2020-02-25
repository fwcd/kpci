{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe (isNothing)
import System.Exit
import Test.QuickCheck

import Arbitraries
import Subst
import Type
import Unification
import TestUtils
import Vars

-- Ensures that a non-empty list is never disjoint to itself
prop_disjointNotReflexive :: [Int] -> Property
prop_disjointNotReflexive xs = (not $ null xs) ==> (not $ disjoint xs xs)

-- Empty substitution should not change term.
prop_substIdentity :: Term -> Property
prop_substIdentity t = collect (depth t) $ collect (distinctVarCount t) $ isNothing $ ds t $ apply empty t

-- Substituted variables should not occur in the output term.
prop_remainingVarsAfterSubst :: Subst -> Term -> Bool
prop_remainingVarsAfterSubst s@(Subst ls) t = disjoint (map fst ls) (allVars $ apply s t)

return []

main :: IO ()
main = do
  result <- $quickCheckAll
  if result
    then exitSuccess
    else exitFailure
