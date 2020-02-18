module Rename where

import State
import Subst
import Type

-- Renames variables for SLD-resolution by renaming precisely those
-- that occur in the given list.
rename :: [VarName] -> Rule -> Rule
rename = undefined

-- Renames all anonymous variables in a term.
renameAnonymous :: Term -> State ([VarName], [VarName]) Term
renameAnonymous (Var x) | x == "_"  = do ((v:vs), used) <- get
                                         set (vs, v:used)
                                         return v
                        | otherwise = return $ Var x
renameAnonymous (Comb f ts) = fmap (Comb f) $ sequence $ map renameAnonymous ts
