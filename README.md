# The Kiel Prolog Compiler Interactive
A glorious Prolog environment written by Hendrik and Fredrik.

[![CI](https://github.com/fwcd/kpci/workflows/CI/badge.svg)](https://github.com/fwcd/kpci/actions)

## Running
To run, you will need `cabal-install`. Once installed, just run `cabal v2-run` to open the Prolog REPL.

## Usage
You can try a simple example by loading a Prolog file from the `examples` folder and entering a query:

```
:l examples/last.pl
last([1, 2, 3], X).
```

## Testing
To execute the test suite, run `cabal v2-test`.
