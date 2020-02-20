module SLD where

import Data.Maybe (maybeToList)
import Rename
import Subst
import Type
import Unification
import Vars

-- An SLD resolution tree.
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving Show

-- A strategy for traversing an SLDTree.
type Strategy = SLDTree -> [Subst]

-- Constructs an SLD tree from a program and a query.
sld :: Prog -> Goal -> SLDTree
sld (Prog prog) (Goal goal) = sld' (goal >>= allVars) (Goal goal)
  where sld' :: [VarName] -> Goal -> SLDTree
        sld' used (Goal g) = SLDTree (Goal g) $ do
          (ls1, l, ls2) <- splitEverywhere g
          r  <- prog
          let (Rule t ts, used') = rename used r
          s  <- maybeToList $ unify l t
          let ls = ls1 ++ ts ++ ls2
          return (s, sld' used' ( Goal $ map (apply s) ls))

-- Splits a list at every position and returns for each split the
-- prefix, the element and the postfix.
splitEverywhere :: [a] -> [([a], a, [a])]
splitEverywhere [] = []
splitEverywhere (x:xs) = ([], x, xs) : map (\(ws, y, zs) -> (x:ws, y, zs)) (splitEverywhere xs)
