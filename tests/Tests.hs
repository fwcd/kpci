{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import System.Exit

prop_simple :: Int -> Bool
prop_simple x = 1 == 2

prop_test :: Bool
prop_test = 1 == 4

return []

main :: IO ()
main = do
  result <- $quickCheckAll
  if result
    then exitSuccess
    else exitFailure
