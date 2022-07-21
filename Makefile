bin := s
path := $(shell stack path --local-install-root)/bin

.PHONY: build clean test doc opendoc
build:
 @echo $(path)
 stack build
 cp -f $(path)/$(bin) app/
 /usr/bin/strip app/$(bin)

clean:
 stack clean
 -/bin/rm -f $(bin)

test:
 stack test

doc:
 stack haddock

opendoc:
 open $(shell stack path --local-doc-root)/index.html
