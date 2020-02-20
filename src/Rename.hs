module Rename (rename) where

import State
import Subst
import Type
import Vars

-- Renames variables for SLD-resolution by renaming precisely those
-- that occur in the given list.
rename :: [VarName] -> Rule -> (Rule, [VarName])
rename used r = (r', used')
    where (r', (_, used')) = runState (renameInRule r) (1, used)

-- Renames variables using unique variable names in a rule.
renameInRule :: Rule -> State (Int, [VarName]) Rule
renameInRule (Rule t ts) = do
    let vs = allVars t ++ (ts >>= allVars)
    vs' <- sequence $ map renameVarName vs

    let s = Subst $ zip vs $ map Var vs'
    t' <- renameApply s t
    ts' <- sequence $ map (renameApply s) ts

    return $ Rule t' ts'

-- Applies a substitution and renames anonymous variables in a term.
renameApply :: Subst -> Term -> State (Int, [VarName]) Term
renameApply s t = do
    t' <- renameAnonymous t
    return $ apply s t'

-- Renames all anonymous variables in a term.
renameAnonymous :: Term -> State (Int, [VarName]) Term
renameAnonymous (Var s) | s == "_"  = fmap Var $ renameVarName s
                        | otherwise = return $ Var s
renameAnonymous (Comb f ts) = fmap (Comb f) $ sequence $ map renameAnonymous ts

-- Makes a variable name unique by appending a number.
renameVarName :: VarName -> State (Int, [VarName]) VarName
renameVarName s = do
  (n, used) <- get
  let newS = renameVarName' s n used
  put (n, newS : used)
  return newS
  where renameVarName' :: VarName -> Int -> [VarName] -> VarName
        renameVarName' s' i used' | elem s'' used' = renameVarName' s' (i + 1) used'
                                  | otherwise      = s''
          where s'' = s' ++ show i

-- The beginning of the list and the longest suffix
-- of elements satisfying the predicate.
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd f xs = reverseResult $ span f $ reverse xs
  where reverseResult (p, s) = (reverse s, reverse p)
