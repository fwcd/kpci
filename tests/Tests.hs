{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

prop_simple :: Int -> Bool
prop_simple x = x == x

return []

main :: IO ()
main = do
  result <- $quickCheckAll
  putStrLn $ show result
