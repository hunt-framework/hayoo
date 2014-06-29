

DEFAULT_PROFOPTS = --enable-executable-profiling --enable-library-profiling --ghc-option=-auto-all --ghc-option=-caf-all
DEFAULT_RUNPOPTS  = +RTS -xc


export PROF=0

ifeq ($(PROF),1)
    PROFOPTS=$(DEFAULT_PROFOPTS)
    RUNPOPTS=$(DEFAULT_RUNPOPTS)
else
    PROFOPTS=
    RUNPOPTS=
endif


install: about.html
	cabal install $(PROFOPTS)

build: about.html
	cabal build $(PROFOPTS)

run: about.html
	cabal run --ghc-option=-auto-all --ghc-option=-caf-all -- $(RUNPOPTS)

about.html: ../README.md
		pandoc -t html -o about.html  --email-obfuscation=references ../README.md
