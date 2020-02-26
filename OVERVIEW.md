# Overview
The general interpreter pipeline is as follows:

* Parsing (implemented in [Parser.hs](src/Parser.hs))
* SLD-Resolution (implemented in [SLD.hs](src/SLD.hs))
  * uses Unification (implemented in [Unification.hs](src/Unification.hs))
    * uses Disagreement Sets (implemented in [Unification.hs](src/Unification.hs))
    * uses Substitutions (implemented in [Subst.hs](src/Subst.hs))
* Tree searching (implemented in [SLD.hs](src/SLD.hs))

A more detailed description of the individual stages can be found below.

## Parsing
_see [Parser.hs](src/Parser.hs)_

Parsing is handled by monadic parser combinations from the `Parsec` library. A subset of the Prolog syntax is supported.

## SLD-Resolution
_see [SLD.hs](src/SLD.hs)_

SLD-Resolution is the algorithm that proves a goal (= query).

It does so by repeatedly replacing the leftmost literal in the goal by it's definition (a rule unifiable with the literal). Since a literal might have multiple definitions, each of these will be represented by a branch in the SLD tree. Each branch additionally has an associated substitution: The unificator computed in the previous step.

Once resolution has been completed, the (leaf) nodes that contain an empty goal represent a successful proof, since every literal could be repeatedly resolved against the program's rules.

### Unification
_see [Unification.hs](src/Unification.hs)_

Unification is the algorithm that, given two terms, finds a substitution that, when applied to each of the terms, yields the _same_ term on both sides. This substitution is called the _unificator_.

To compute a unification, the algorithm repeatedly computes the _disagreement set_ (a pair) of the left and the right term:

* If the disagreement set has a variable on one side, substitute this variable by the right-hand side and recurse the unification algorithm (re-compute the disagreement set, ...)
* Otherwise the unification failed

### Substitution
_see [Subst.hs](src/Subst.hs)_

Substitutions are finite functions that replace certain variable names by certain terms in a term. They are represented by a `variable name -> term` mapping table.

## Tree searching
_see [SLD.hs](src/SLD.hs)_

A tree search is an algorithm that traverses the SLD tree and visits all successful leaf nodes (empty goal). For each successful leaf node, the composed substitution along the path from the leaf to the root is computed and added to the list of results.

This list of resulting substitutions represents the possible solutions for the user's goal (query).
