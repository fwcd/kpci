module Pretty (Pretty (..)) where

import Data.List (intercalate)
import Type

class Pretty a where
  -- Renders to normal Prolog syntax.
  pretty :: a -> String

instance Pretty Term where
  pretty (Var name) = name
  pretty (Comb "." [x, y]) = "[" ++ prettyRecursive x y ++ "]"
    where prettyRecursive :: Term -> Term -> String
          prettyRecursive t (Comb "[]" [])    = pretty t
          prettyRecursive t (Comb "." [z, w]) = pretty t ++ ", " ++ prettyRecursive z w
          prettyRecursive t t'                = pretty t ++ "|" ++ pretty t'
  pretty (Comb name []) = name
  pretty (Comb name ts) = name ++ "(" ++ (intercalate ", " (map pretty ts)) ++ ")"

instance Pretty Rule where
  pretty (Rule t ts) = (pretty t) ++ " :- " ++ (intercalate ", " (map pretty ts)) ++ "."
  
instance Pretty Goal where
  pretty (Goal ts) = (intercalate ", " $ map pretty ts) ++ "."
