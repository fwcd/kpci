{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe (isNothing)
import System.Exit
import Test.QuickCheck

import Subst
import Type
import Unification

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

depth :: Term -> Int
depth (Var _) = 0
depth (Comb _ ts) = 1 + (foldr max 0 $ map depth ts)

prop_substIdentity :: Term -> Property
prop_substIdentity t = collect (depth t) $ isNothing $ ds t $ apply empty t

return []

main :: IO ()
main = do
  result <- $quickCheckAll
  if result
    then exitSuccess
    else exitFailure
