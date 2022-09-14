bin := s
path := $(shell stack path --local-install-root)/bin

.PHONY: build clean test bench doc opendoc
build:
	@echo $(path)
	stack build
	cp -f $(path)/$(bin) app/
	/usr/bin/strip app/$(bin)

clean:
	git clean -xdf
	stack clean --full
	-/bin/rm -f $(bin)

test:
	stack test

bench:
	stack bench

doc:
	stack haddock

opendoc:
	open $(shell stack path --local-doc-root)/index.html
