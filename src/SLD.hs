module SLD (SLDTree (..), Strategy, sld) where

import Data.Maybe (maybeToList)
import Pretty
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
        sld' used (Goal []) = SLDTree (Goal []) []
        sld' used g@(Goal (l:ls)) = SLDTree g $ do
          r <- prog
          let (Rule t ts, used') = rename used r
          s  <- maybeToList $ unify l t
          return (s, sld' used' $ Goal $ map (apply s) $ ts ++ ls)

-- Splits a list at every position and returns for each split the
-- prefix, the element and the postfix.
splitEverywhere :: [a] -> [([a], a, [a])]
splitEverywhere [] = []
splitEverywhere (x:xs) = ([], x, xs) : map (\(ws, y, zs) -> (x:ws, y, zs)) (splitEverywhere xs)

instance Pretty SLDTree where
  pretty t = unlines $ pretty' t
    where pretty' :: SLDTree -> [String]
          pretty' (SLDTree g cs) = pretty g : (map ("  " ++) $ cs >>= prettyChild)
          prettyChild :: (Subst, SLDTree) -> [String]
          prettyChild (s, t) = (arr ++ pretty s) : (map ((flip replicate ' ' $ length arr) ++) $ pretty' t)
            where arr = "=> "
