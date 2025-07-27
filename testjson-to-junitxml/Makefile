OBJS=$(subst .hs,,$(wildcard *.hs))

$(OBJS): %: %.hs
	ghc $<

.PHONY: run 
run: $(OBJS)
	./hello

.PHONY: deps
deps:
	cabal install --lib aeson bytestring # no clue if `cabal install --lib` is good or bad behaviour
	cabal install --lib text-show
	cabal install --lib text
