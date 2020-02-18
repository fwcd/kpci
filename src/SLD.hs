module SLD where

import Subst
import Type

-- An SLD resolution tree.
data SLDTree = SLDTree Goal [(Subst, SLDTree)]

-- A strategy for traversing an SLDTree.
type Strategy = SLDTree -> [Subst]
