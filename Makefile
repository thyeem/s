bin := s
cabal-path := $(shell cabal list-bin $(bin))

.PHONY: build clean test bench doc opendoc
build:
	cabal build
	cp -f $(cabal-path) app/
# -/usr/bin/strip app/$(bin)

clean:
	git clean -xdf
	cabal clean -v3

test:
	cabal build --enable-tests && cabal exec -- cabal test

bench:
	cabal build --enable-benchmarks && cabal bench

doc:
	cabal haddock

opendoc:
	open $(shell /usr/bin/find dist-newstyle -name "index.html")
