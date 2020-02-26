module TestUtils (
  depth,
  disjoint,
  distinctVarCount,
  trim,
  liftEither,
  (<.$>),
  (<$.>),
  mapLeft,
  pair,
  padRight,
  formatMessages) where

import Control.Monad.Trans.Except
import Data.Either (either)
import Data.List (nub)

import Type
import Vars

-- Fetches the maximum recursion depth of a given term.
depth :: Term -> Int
depth (Var _) = 0
depth (Comb _ ts) = 1 + (foldr max 0 $ map depth ts)

-- Tests whether two lists do not share elements.
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = foldr (\y -> ((not $ elem y xs) &&)) True

-- Fetches the number of distinct variables in a given term.
distinctVarCount :: Term -> Int
distinctVarCount = length . nub . allVars

-- Trims whitespace from a string.
trim :: String -> String
trim = t . t
  where t = dropWhile (== ' ') . reverse

-- Lifts an either into the ExceptT transformer.
liftEither :: Monad m => Either a b -> ExceptT a m b
liftEither = either throwE return

-- Maps over the left element of a tuple.
(<.$>) :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
(<.$>) g = fmap $ \(x, y) -> (g x, y)

-- Maps over the right element of a tuple.
(<$.>) :: Functor f => (a -> b) -> f (c, a) -> f (c, b)
(<$.>) g = fmap $ \(x, y) -> (x, g y)

-- Maps over the error case in an Either.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left e)  = Left $ f e
mapLeft _ (Right x) = Right x

-- Creates a 2-tuple.
pair :: a -> b -> (a, b)
pair x y = (x, y)

-- Pads a string on the right.
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 $ n - len) ' '
  where len = length s

-- Formats a list of messages by padding.
formatMessages :: [(String, String)] -> [String]
formatMessages msgs = uncurry (++) <$> (padRight offs <.$> msgs)
  where offs = 2 + (foldr max 0 $ length <$> fst <$> msgs)
