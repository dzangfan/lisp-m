
# Lisp in [DOI:10.1145/367177.367199](https://dl.acm.org/doi/10.1145/367177.367199)

## Build and run

Basic GHC environment is required. Tested in ghc-9.4.8 and cabal-3.10.3.0.

```shell
cabal build
cabal exec lisp-m                     # Starting a REPL
cabal exec lisp-m -- -l stdlib.lisp-m # Loading a file and starting a REPL
```

See more options by `cabal exec lisp-m -- --help`.

## Changes in Syntax

The syntax (and some aspects of semantics) is slightly different from the paper. For m-exps,

- By convention, `NIL` is used instead of `F`.
- Conditions are written as `{ M -> M; M -> M; ... }`
- No lambda notations and labels. Everything is first-order. Functions (and recursive functions) are defined by `f[x; y; ...] = rhs`.
- Macros are added. They are defined by `#m[x; y; ...] = rhs` and used by `#m[x; y; ...]`. Note that the macros are simple replacement (or inlining) rather than those in Common Lisp / Scheme.

For s-exps,

- By convention (of recent lisps), commas should appear in s-exps.
