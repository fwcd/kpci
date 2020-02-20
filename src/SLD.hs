module SLD (SLDTree (..), Strategy, sld, strategies, solve) where

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

instance Pretty SLDTree where
  pretty t = unlines $ pretty' t
    where pretty' :: SLDTree -> [String]
          pretty' (SLDTree g cs) = pretty g : (map ("  " ++) $ cs >>= prettyChild)
          prettyChild :: (Subst, SLDTree) -> [String]
          prettyChild (s, t) = (arr ++ pretty s) : (map ((flip replicate ' ' $ length arr) ++) $ pretty' t)
            where arr = "=> "

-- Performs a depth-first search on the SLD tree.
dfs :: Strategy
dfs = dfs' empty
  where dfs' :: Subst -> Strategy
        dfs' s (SLDTree (Goal (_:_)) []) = []
        dfs' s (SLDTree _ cs) = cs >>= (dfsChild s)
        dfsChild :: Subst -> (Subst, SLDTree) -> [Subst]
        dfsChild s1 (s2, t) = dfs' (compose s2 s1) t

-- Maps strategy names to strategies.
strategies :: [(String, Strategy)]
strategies = [("dfs", dfs)]

-- Solves a goal using a program and a strategy.
solve :: Strategy -> Prog -> Goal -> [Subst]
solve strat p g@(Goal ls) = map (\(Subst subs) -> Subst $ filter (flip elem vs . fst) subs) $ strat $ sld p g
  where vs = ls >>= allVars
