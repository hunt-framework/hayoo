

SERVER  = http://localhost:3000

json:
	$(MAKE) -C hayoo-json all

insert: 
	$(MAKE) -C hayoo-json insert SERVER=$(SERVER)

sandbox:
	cd hayooLib       && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox
	cd hayooFrontend  && cabal sandbox init --sandbox ../../hunt/.cabal-sandbox

.PHONY: all clean json

all: 

clean:

