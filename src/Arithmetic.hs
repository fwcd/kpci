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
eval (Comb "div" ts) = do es <- mapM eval ts
                          if elem 0 $ tail es then Nothing
                                              else foldl1Safe div es
eval (Comb "mod" ts) = do es <- mapM eval ts
                          if elem 0 $ tail es then Nothing
                                              else foldl1Safe mod es
eval (Comb value _)  = readMaybe value
eval _               = Nothing
