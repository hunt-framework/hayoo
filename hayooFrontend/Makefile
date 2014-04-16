

PROFOPTS = --enable-executable-profiling --enable-library-profiling --ghc-option=-auto-all --ghc-option=-caf-all
RUNPOTS  = +RTS -xc


install:
	cabal install $(PROFOPTS)

build:
	cabal build $(PROFOPTS)

run:
	cabal run --ghc-option=-auto-all --ghc-option=-caf-all -- $(RUNPOTS)
