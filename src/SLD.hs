module SLD (SLDTree (..), Strategy, sld, strategies, defaultStrategy, solve) where

import Data.Maybe (maybeToList, listToMaybe, isJust)
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
-- The strategy is needed for the negation-as-failure operator.
sld :: Strategy -> Prog -> Goal -> SLDTree
sld strat (Prog prog) (Goal goal) = sld' (goal >>= allVars) (Goal goal)
  where sld' :: [VarName] -> Goal -> SLDTree
        sld' _      (Goal [])     = SLDTree (Goal []) []
        sld' used g@(Goal (l:ls)) = SLDTree g $ do
          r <- prog
          let (Rule t ts, used') = rename used r

          l' <- case l of
                    (Comb "call" (Comb p args:args')) -> [Comb p $ args ++ args']
                    (Comb "\\+"  [t'])                -> if isJust $ do s <- unify t' t
                                                                        listToMaybe $ strat $ sld' used' $ Goal $ apply s <$> ts ++ ls
                                                           then [l]
                                                           else []
                    _                                 -> [l]
          s <- maybeToList $ unify l' t

          return (s, sld' used' $ Goal $ apply s <$> ts ++ ls)

instance Pretty SLDTree where
  pretty = unlines . pretty'
    where pretty' :: SLDTree -> [String]
          pretty' (SLDTree g cs) = pretty g : (map ("  " ++) $ cs >>= prettyChild)
          prettyChild :: (Subst, SLDTree) -> [String]
          prettyChild (s, t) = (arr ++ pretty s) : (map ((flip replicate ' ' $ length arr) ++) $ pretty' t)
            where arr = "=> "

-- Finds the height of the tree.
height :: SLDTree -> Int
height (SLDTree _ []) = 1
height (SLDTree _ cs) = 1 + (foldr max 0 $ height <$> snd <$> cs)

-- Performs a depth-first search on the SLD tree.
-- The central idea of the algorithm is to recursively
-- carry an accumulated substitution around.
dfs :: Strategy
dfs = dfs' empty
  where dfs' :: Subst -> Strategy
        dfs' s (SLDTree (Goal [])    []) = [s] -- Successful leaf node
        dfs' _ (SLDTree (Goal (_:_)) []) = []  -- Failure leaf node
        dfs' s (SLDTree _ cs)            = cs >>= (dfsChild s)
        dfsChild :: Subst -> (Subst, SLDTree) -> [Subst]
        dfsChild s1 (s2, t) = dfs' (compose s2 s1) t

-- Performs a breadth-first search on the SLD tree.
-- The central idea of the algorithm is to carry an
-- accumulated substitution in the BFS queue.
bfs :: Strategy
bfs t = bfs' [(empty,t)]
  where bfs' :: [(Subst, SLDTree)] -> [Subst]
        bfs' []                                = []
        bfs' ((subst, SLDTree (Goal g) cs):ts) = s ++ bfs' (ts ++ (composeMap subst cs))
          where s = case (g, cs) of
                      ([], []) -> [subst]
                      _        -> []
        composeMap :: Subst -> [(Subst, SLDTree)] -> [(Subst, SLDTree)]
        composeMap _ []                 = []
        composeMap s ((subst, tree):ts) = (compose subst s, tree): composeMap s ts

-- Performs a depth-first search that only finds the leaves at the specified depth.
dfsN :: Int -> Strategy
dfsN = flip dfsN' empty
  where dfsN' :: Int -> Subst -> Strategy
        dfsN' n s (SLDTree (Goal [])    []) | n == 0    = [s]
                                            | otherwise = []
        dfsN' _ _ (SLDTree (Goal (_:_)) [])             = []
        dfsN' n s (SLDTree _ cs)            | n <= 0    = []
                                            | otherwise = cs >>= (dfsChildN n s)
        dfsChildN :: Int -> Subst -> (Subst, SLDTree) -> [Subst]
        dfsChildN n s1 (s2, t) = dfsN' (n - 1) (compose s2 s1) t

-- Performs an iteratively deepening depth-first search on the SLD tree
iddfs :: Strategy
iddfs t = [0..height t] >>= flip dfsN t

-- Maps strategy names to strategies.
strategies :: [(String, Strategy)]
strategies = [("dfs", dfs), ("bfs", bfs), ("iddfs", iddfs)]

-- A default strategy.
defaultStrategy :: Strategy
defaultStrategy = dfs

-- Solves a goal using a program and a strategy.
solve :: Strategy -> Prog -> Goal -> [Subst]
solve strat p g@(Goal ls) = map (\(Subst subs) -> Subst $ filter (flip elem vs . fst) subs) $ strat $ sld strat p g
  where vs = ls >>= allVars
