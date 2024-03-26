# s

`s` is a generalized parser combinator *extremely easy to use*. The letter `s` symbolizes _simplicity_.

This project is heavily inspired by [`Parsec`](https://hackage.haskell.org/package/parsec), but has a completely **different goal**.   
`s` focuses more on the essentials so that it helps _usability_ and _extensibility_.

### Build
```bash
# not uploaded to the hackage yet
$ git clone https://github.com/thyeem/s.git    
$ stack build

# doctest
$ stack test

# generate/open doc
$ stack haddock --open s
```

### Examples

#### sLISP 
See [`sLISP`](https://github.com/thyeem/slisp), which is a pure-functional `LISP` implementation in `Haskell`.   
`s` is used in the parsing part of _S-expression_ in `sLISP`.

#### CSV parser
_See `src/Text/S/Example/CSV.hs`_

#### JSON parser
_See `src/Text/S/Example/JSON.hs`_

#### Generalized calculator
_See `src/Text/S/Example/Calc.hs`_

##### calculator REPL
```haskell
>>> import Text.S.Example.Calc (calc)
>>> calc
Choose an Expr Calculators [infixl, infixr, prefix, postfix]: 
```


