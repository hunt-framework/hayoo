
# disable prarallel builds.
.NOTPARALLEL:

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

.PHONY: all clean  

all:
	@echo "usage:"
	@echo "run  'first-install' first"
	@echo "then 'first-run' to start the hunt server + the hayoo frontend"
	@echo "use  'run' after modifiying the frontend"

sandbox: ../hunt
	cd hayooLib       && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox
	cd hayooFrontend  && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox
	cd hayooIndexer   && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox
	cd ../hunt/ && cabal sandbox add-source ../hayoo/hayooLib 

install: hayooLib-install hayooFrontend-install hayooIndexer-install

clean:
	cd hayooLib       && cabal clean
	cd hayooFrontend  && cabal clean

../hunt/.cabal-sandbox/bin/hunt-server-cli:
	cd ../hunt && make hunt-server-cli

start-hunt-scotty: index/hayoo.index.json ../hunt/.cabal-sandbox/bin/hunt-server-cli
	cd ../hunt && make startServer
	sleep 1 && ../hunt/.cabal-sandbox/bin/hunt-server-cli eval index/hayoo.index.json

first-run: start-hunt-scotty run

run: hayooFrontend-run

first-install: sandbox install

hayooFrontend-configure:
	cd hayooFrontend && cabal configure $(PROFOPTS)

hayooFrontend-install: hayooFrontend/about.html hayooFrontend/examples.html 
	cd hayooFrontend && cabal install $(PROFOPTS)

hayooFrontend-build: hayooFrontend/about.html hayooFrontend/examples.html
	cd hayooFrontend && cabal build $(PROFOPTS)

# --ghc-option=-auto-all --ghc-option=-caf-all -- $(RUNPOPTS)
hayooFrontend-run: hayooFrontend/about.html hayooFrontend/examples.html
	cd hayooFrontend && cabal run

hayooFrontend/about.html: README.md
	pandoc -t html -o hayooFrontend/about.html  --email-obfuscation=references README.md

hayooFrontend/examples.html: Examples.md
	pandoc -t html -o hayooFrontend/examples.html Examples.md

hayooLib-install: 
	cd hayooLib       && cabal install

hayooIndexer-install: 
	cd hayooIndexer   && cabal install

binary-package:
	cd hayooFrontend       && (cabal configure --prefix=/usr/local && cabal build && cabal copy --destdir=/tmp/hayoo)
	cd hayooIndexer        && (cabal configure --prefix=/usr/local && cabal build && cabal copy --destdir=/tmp/hayoo)
	cd ../hunt/hunt-server && (cabal configure --prefix=/usr/local && cabal build && cabal copy --destdir=/tmp/hayoo) 
	tar --transform 's,^tmp/hayoo/,,S' -czf hayoo.tar.gz /tmp/hayoo/

index/hayoo.index.tar.gz:
	mkdir -p index
	curl -s "http://hayoo.fh-wedel.de/download/hayoo-tutorial.tar.gz" > index/hayoo.index.tar.gz

index/hayoo.index.json: index/hayoo.index.tar.gz
	cd index && tar -xvzf hayoo.index.tar.gz



clean:



