module Arbitraries () where

import Test.QuickCheck

import Subst
import Type

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
