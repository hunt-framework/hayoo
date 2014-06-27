
# disable prarallel builds.
.NOTPARALLEL:
	
SERVER  = http://localhost:3000

json:
	$(MAKE) -C hayoo-json all

insert: 
	$(MAKE) -C hayoo-json insert SERVER=$(SERVER)

sandbox:
	cd hayooLib       && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox
	cd hayooFrontend  && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox
	cd ../../hunt/ && cabal sandbox add-source hayooLib 

binary-package:
	cd hayooFrontend       && (cabal configure --prefix=/usr/local && cabal build && cabal copy --destdir=/tmp/hayoo)
	cd ../hunt/hunt-server && (cabal configure --prefix=/usr/local && cabal build && cabal copy --destdir=/tmp/hayoo) 
	tar --transform 's,^tmp/hayoo/,,S' -czf hayoo.tar.gz /tmp/hayoo/

install:
	cd hayooLib       && cabal install
	cd hayooFrontend  && cabal install

first-install: sandbox install

.PHONY: all clean json sandbox binary-package install

all: 

clean:

