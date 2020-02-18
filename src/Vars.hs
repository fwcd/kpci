module Vars (Vars (..), freshVars) where

import Type

class Vars a where
  -- Extracts all variables from a term
  allVars :: a -> [VarName]

instance Vars Term where
  allVars (Var name) = [name]
  allVars (Comb _ ts) = ts >>= allVars

instance Vars Rule where
  allVars (Rule t ts) = allVars t ++ (ts >>= allVars)

instance Vars Prog where
  allVars (Prog rs) = rs >>= allVars

instance Vars Goal where
  allVars (Goal ts) = ts >>= allVars

-- An infinite supply of fresh, possible variables
freshVars :: [VarName]
freshVars = filter isValid
          $ filter (not . null)
          $ allCombinations
          $ lowers ++ uppers ++ numbers ++ "_"
    where lowers = ['a' .. 'z']
          uppers = ['A' .. 'Z']
          numbers = ['1' .. '9']
          isValid :: VarName -> Bool
          isValid = (flip elem (uppers ++ "_")) . head

-- Generates all possible finite non-empty combination lists.
allCombinations :: [a] -> [[a]]
allCombinations [] = [[]]
allCombinations xs = [] : (allCombinations xs >>= \w -> map (:w) xs)
