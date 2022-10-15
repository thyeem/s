bin := s
ghc := --with-compiler=ghc-8.10.7
opts := --ghc-options="-Wall -Wno-name-shadowing -Wno-orphans"
fast := $(ghc) --ghc-options=-O0 $(opts)
release := $(ghc) --ghc-options="-O2 -fexpose-all-unfoldings" $(opts)
test-opts := $(fast) --enable-tests --test-show-details=direct

.PHONY: build
build:
	cabal build $(fast)
	cp -f $(shell cabal list-bin $(bin)) app

release:
	cabal build $(release)
	cp -f $(shell cabal list-bin $(bin)) app

.PHONY: clean
clean:
	git clean -xdf
	cabal clean

.PHONY: tests
tests:
	make doctest
	make test

.PHONY: test
test:
	cabal test test $(test-opts) --test-option=--match --test-option="$(match)"

.PHONY: doctest
doctest:
	cabal exec -- \
	cabal test doctest $(test-opts)

.PHONY: bench
bench:
	cabal build $(release) --enable-benchmarks && cabal bench

.PHONY: doc
doc:
	cabal haddock --haddock-hyperlink-source

.PHONY: opendoc
opendoc:
	open $(shell /usr/bin/find dist-newstyle -name "index.html")
