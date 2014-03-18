

SERVER  = http://localhost:3000

json:
	$(MAKE) -C hayoo-json all

insert: 
	$(MAKE) -C hayoo-json insert SERVER=$(SERVER)

sandbox:
	cd hayooLib       && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox
	cd hayooFrontend  && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox

binary-package:
	cd hayooFrontend       && (cabal configure --prefix=/usr/local && cabal build && cabal copy --destdir=/tmp/hayoo)
	cd ../hunt/hunt-server && (cabal configure --prefix=/usr/local && cabal build && cabal copy --destdir=/tmp/hayoo) 
	tar --transform 's,^tmp/hayoo/,,S' -czf hayoo.tar.gz /tmp/hayoo/

.PHONY: all clean json sandbox binary-package

all: 

clean:

