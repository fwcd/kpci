# The Kiel Prolog Compiler Interactive

[![CI](https://github.com/fwcd/kpci/workflows/CI/badge.svg)](https://github.com/fwcd/kpci/actions)

An interactive Prolog interpreter written by Hendrik and Fredrik.

The interpreter uses unification, SLD resolution and various tree search algorithm (DFS, BFS, IDDFS) to find a set of solutions given a query. A more detailed description can be found in [OVERVIEW.md](OVERVIEW.md).

More advanced features that are supported:

* Higher-order predicates (`call`)
* Negation-as-failure (`\+`)
* Arithmetic (`is`)

Not implemented yet:

* Encapsulation of nondeterminism (`findall`)

## Project Structure
```
app:       Executable sources (Haskell)
examples:  Prolog sources
  rules:   Prolog (rule) files
  tests:   Lists of Prolog queries to be asserted
resources: Additional resources
src:       Library sources (Haskell)
tests:     Test suite sources (Haskell)
```

## Running
To run, you will need `cabal-install`. Once installed, just run `cabal v2-run` to open the Prolog REPL.

## Usage
You can try a simple example by loading a Prolog file from the `examples/rules` folder and entering a query:

```
:l examples/rules/last.pl
last([1, 2, 3], X).
```

## Testing
To execute the test suite, run `cabal v2-test`.
