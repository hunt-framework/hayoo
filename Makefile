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

all:
	@echo "usage:"
	@echo "run  'make install' for starting stack install"
	@echo "then start hunt and hayoo server"
	@echo "put ~/.local/bin into your PATH"

install:
	$(MAKE) hayooFrontend/about.html
	$(MAKE) hayooFrontend/examples.html
	stack setup
	stack install

clean:
	stack clean

start-hunt-scotty: index/hayoo.index.json
	cd ../hunt && make startServer
	sleep 1 && hunt-server-cli eval index/hayoo.index.json

first-run: start-hunt-scotty run

run: hayooFrontend-run

hayooFrontend/about.html: README.md
	pandoc -t html -o hayooFrontend/about.html  --email-obfuscation=references README.md

hayooFrontend/examples.html: Examples.md
	pandoc -t html -o hayooFrontend/examples.html Examples.md

index/hayoo.index.tar.gz:
	mkdir -p index
	curl -s "http://hayoo.fh-wedel.de/download/hayoo-tutorial.tar.gz" > index/hayoo.index.tar.gz

index/hayoo.index.json: index/hayoo.index.tar.gz
	cd index && tar -xvzf hayoo.index.tar.gz

.PHONY	: all install clean start-hunt-scotty first-run run
