

PROFOPTS = --enable-executable-profiling --disable-library-profiling --ghc-option=-auto-all --ghc-option=-caf-all
RUNPOTS  = +RTS -xc


install:
	cabal install $(PROFOPTS)

run:
	cabal run -- $(RUNPOTS)