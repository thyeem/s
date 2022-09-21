bin := s

.PHONY: build
build:
	cabal build
	cp -f $(shell cabal list-bin $(bin)) app

.PHONY: clean
clean:
	git clean -xdf
	cabal clean

.PHONY: test
test:
	cabal build --enable-tests && cabal exec -- cabal test

.PHONY: bench
bench:
	cabal build --enable-benchmarks && cabal bench

.PHONY: doc
doc:
	cabal haddock --haddock-hyperlink-source

.PHONY: opendoc
opendoc:
	open $(shell /usr/bin/find dist-newstyle -name "index.html")
