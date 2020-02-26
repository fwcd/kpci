{-# LANGUAGE TemplateHaskell #-}
module QCTests (runAllQCTests) where

import Data.Maybe (isNothing)
import Test.QuickCheck

import Subst
import Type
import Unification
import TestUtils
import Vars

instance Arbitrary Term where
  arbitrary = do
    n <- choose (0 :: Int, 4 :: Int)
    name <- arbitrary
    k <- choose (1 :: Int, 8 :: Int)
    ts <- sequence [arbitrary | _ <- [1..k]]
    return $ if n == 0
               then Comb name ts
               else Var name 

instance Arbitrary Subst where
  arbitrary = do
    ls <- arbitrary
    return $ Subst ls

-- Ensures that a non-empty list is never disjoint to itself
prop_disjointNotReflexive :: [Int] -> Property
prop_disjointNotReflexive xs = (not $ null xs) ==> (not $ disjoint xs xs)

-- Empty substitution should not change term.
prop_substIdentity :: Term -> Property
prop_substIdentity t = collect (depth t)
                     $ collect (distinctVarCount t)
                     $ isNothing $ ds t $ apply empty t

-- Substituted variables should not occur in the output term.
prop_remainingVarsAfterSubst :: Subst -> Term -> Property
prop_remainingVarsAfterSubst s@(Subst subs) t = isValidSubst ==> disjoint (fst <$> subs) (allVars $ apply s t)
  where (ls, rs) = unzip subs
        isValidSubst = disjoint ls $ rs >>= allVars

return []

runAllQCTests :: IO Bool
runAllQCTests = $quickCheckAll
