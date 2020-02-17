module Vars where

import Type

class Vars a where
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
