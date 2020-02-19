module Rename (rename) where

import State
import Subst
import Type
import Vars

-- Renames variables for SLD-resolution by renaming precisely those
-- that occur in the given list.
rename :: [VarName] -> Rule -> (Rule, [VarName])
rename used r = (r', used')
    where (r', (_, used')) = runState (renameInRule r) (filter (not . flip elem used) $ freshVars, used)

-- Renames variables using unique variable names in a rule.
renameInRule :: Rule -> State ([VarName], [VarName]) Rule
renameInRule (Rule t ts) = do
    (fresh, used) <- get

    let vs = allVars t ++ (ts >>= allVars)
        (fs, fs') = splitAt (length vs) fresh
        s = Subst $ zip vs $ map Var fs
    put (fs', used ++ fs)

    t' <- renameApply s t
    ts' <- sequence $ map (renameApply s) ts

    return $ Rule t' ts'

-- Applies a substitution and renames anonymous variables in a term.
renameApply :: Subst -> Term -> State ([VarName], [VarName]) Term
renameApply s t = do
    t' <- renameAnonymous t
    return $ apply s t'

-- Renames all anonymous variables in a term.
renameAnonymous :: Term -> State ([VarName], [VarName]) Term
renameAnonymous (Var x) | x == "_"  = do (fresh, used) <- get
                                         put (tail fresh, head(fresh):used)
                                         return $ Var $ head fresh
                        | otherwise = return $ Var x
renameAnonymous (Comb f ts) = fmap (Comb f) $ sequence $ map renameAnonymous ts
