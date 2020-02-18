module Rename where

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

-- sequence :: [State ... Term] -> State ... [Term]
-- sequence :: Monad m => [m a] -> m [a]
{-

main :: IO ()
main = do
    let xs = [1..]
    let ((x, y, z), array) = runState g xs

g :: State [Int] (Int, Int, Int)
g = do
    firstInt <- f
    secondInt <- f
    thirdInt <- f
    return (firstInt, secondInt, thirdInt)

f :: State [Int] Int
f = do
    (x:xs) <- get
    set xs
    return x

-}
