module Arithmetic (eval) where

import Text.Read (readMaybe)
import Type

-- Safely performs a left-associative fold without a base case.
foldl1Safe :: (a -> a -> a) -> [a] -> Maybe a
foldl1Safe _ [] = Nothing
foldl1Safe f xs = Just $ foldl1 f xs

-- Evaluates an arithmetic expression.
eval :: Term -> Maybe Integer
eval (Comb "+" ts)   = foldr (+) 0 <$> mapM eval ts
eval (Comb "*" ts)   = foldr (*) 1 <$> mapM eval ts
eval (Comb "-" ts)   = foldl1Safe (-) =<< mapM eval ts
eval (Comb "div" ts) = foldl1Safe div =<< mapM eval ts
eval (Comb "mod" ts) = foldl1Safe mod =<< mapM eval ts
eval (Comb value _)  = readMaybe value
eval _               = Nothing
