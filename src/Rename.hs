module Rename where

import State
import Subst
import Type
import Vars

-- Renames variables for SLD-resolution by renaming precisely those
-- that occur in the given list.
rename :: [VarName] -> Rule -> Rule
rename used r = fst $ runState (renameInRule r) (filter (not . flip elem used) $ freshVars, used)

renameInRule :: Rule -> State ([VarName], [VarName]) Rule
renameInRule (Rule t ts) = do
    (fresh, used) <- get

    let vs = allVars t ++ (ts >>= allVars)
        (fs, fs') = splitAt (length vs) fresh
        s = Subst $ zip vs $ map Var fs
    set (fs', used ++ fs)

    t' <- renameApply s t
    ts' <- sequence $ map (renameApply s) ts

    return $ Rule t' ts'

renameApply :: Subst -> Term -> State ([VarName], [VarName]) Term
renameApply s t = do
    t' <- renameAnonymous t
    return $ apply s t'

-- Renames all anonymous variables in a term.
renameAnonymous :: Term -> State ([VarName], [VarName]) Term
renameAnonymous (Var x) | x == "_"  = do ((v:vs), used) <- get
                                         set (vs, v:used)
                                         return $ Var v
                        | otherwise = return $ Var x
renameAnonymous (Comb f ts) = fmap (Comb f) $ sequence $ map renameAnonymous ts
