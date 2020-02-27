module Arithmetic (eval) where

import Text.Read (readMaybe)
import Type

data ArithmeticError = DivByZero | NoValue

-- Safely performs a left-associative fold without a base case.
foldl1Safe :: (a -> a -> a) -> [a] -> Either ArithmeticError a
foldl1Safe _ [] = Left NoValue
foldl1Safe f xs = Right $ foldl1 f xs

-- Evaluates an arithmetic expression.
eval :: Term -> Either ArithmeticError Integer
eval (Comb "+" ts)   = foldr (+) 0 <$> mapM eval ts
eval (Comb "*" ts)   = foldr (*) 1 <$> mapM eval ts
eval (Comb "-" ts)   = foldl1Safe (-) =<< mapM eval ts
eval (Comb "div" ts) = do
  es <- mapM eval ts
  if elem 0 es then Left DivByZero
               else foldl1Safe div es
eval (Comb "mod" ts) = do
  es <- mapM eval ts
  if elem 0 es then Left DivByZero
               else foldl1Safe mod es
eval (Comb value _)  = maybe (Left NoValue) Right $ readMaybe value
eval _               = Left NoValue
