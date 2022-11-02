# s

The `parser S` is a generalized parser combinator extremely easy-to-use. This project is heavily inspired by [`Parsec`](https://hackage.haskell.org/package/parsec).
`parser S` was designed to focus on `USABILITY`, `SCALABILITY` and `NON-VERBOSITY`.

The letter `s` symbolizes simplicity.

## Build
```bash
# Assume the Haskell GHC and cabal is installed

# build: this yields ~80kB binary 'sl' in ./app directory.
$ make release

# doctest
$ make doctest

# generate doc
$ make doc

# open doc
$ make opendoc
```
