{-# LANGUAGE TemplateHaskell #-}

import System.Exit
import Test.QuickCheck

import Subst
import Type

instance Arbitrary Term where
  arbitrary = do
    name <- arbitrary
    ts <- arbitrary
    oneof [return $ Var name, return $ Comb name ts]

instance Arbitrary Subst where
  arbitrary = do
    ls <- arbitrary
    return $ Subst ls

return []

main :: IO ()
main = do
  result <- $quickCheckAll
  if result
    then exitSuccess
    else exitFailure
