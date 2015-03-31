OPTS	=
DAYS	= 10

latest	= 7days
valid	= 1hour
validix	= 1month
threads = 1

# threads = 1 means no parallel indexing but merging of partial indexes as a binary merge

# threads = 20 makes indexing much more efficent than a smaller figure,
# the merging of indexes is more efficient with a higher # of threads
# crawling may be a bit unfriendy to hackage if done with 20 threads
# there is another problem with parallel crawling: curl is not yet thread save

ix	= ix.bin
px	= pkg.bin
ixn	= new-$(ix)
pxn	= new-$(px)

# bindir  = $(HOME)/.cabal/bin
bindir  = ../../../hunt/.cabal-sandbox/bin

indexer	= $(bindir)/hayooIndexer

progs	= $(indexer)

# the -A option is important for garbage collection performance,
# a good value is about the size of the L2 cache of the cpu
# the default is set to 8M

HOST	:= $(shell hostname)

N       = 1
ifeq ($(HOST),holumbus)
N       = 2
endif
ifeq ($(HOST),mecki)
N       = 2
endif

H       = 100
A       = 8
K       = 10
RUNOPTS = +RTS -N$(N) -s -K$(K)M -A$(A)M -H$(H)M -I0 -RTS

# partition: size of partial indexes build with whole-index
#            if size >= 1000, there occur "out of memory" errors when writing partial indexes
# maxpar:    maximum of docs indexed and merged in paralled ( <= partition )
# maxdocs:   upper limit of # of docs, hackage had about 33000 docs (2013-10-09)

partition =500
maxpar    =500
maxdocs   =50000

all	:
	cd .. && cabal install

$(progs)	:
	cd .. && cabal install

force	:
	rm -f $(progs)
	cd .. && cabal clean
	$(MAKE) all

whole-cache	: $(indexer)
	@echo "load hayoo cache from hackage, all package and haddock pages are (re-)loaded,"
	@echo "parsed and stored in binary form in subdir cache"
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< $(RUNOPTS) --cache \
		--hackage \
		--maxdocs=$(maxdocs) \
		--maxthreads=$(threads) \
		--valid=$(valid) \
                $(OPTS)

#----------------------------------------

# uri of hunt server
JHOST   = http://localhost:3000

# indexer options to push results directly into hunt-server
# with --json-maxsave=n the server saves its state after indexing n haddock packges
# for packges and rank the state is saved at the end of indexing

JSERV	= --json-server=$(JHOST)/  --json-maxsave=1000

# default crawler option for json
# --maxdocs=n is the upper limit of pages to be indexed,
# it prevents the crawler running into an infinite loop
#
# --maxtheads=n is the number of haskell threads indexing pages in parallel
# it is set to 1 meaning the crawler is running sequentially
# values > 1 led to segmentation faults, the reason seem, that
# the curl lib is (was?) not thread save
#
# --valid=d is the duration for which the local cache with the hackage pages
# is valid, --valid=0 leads to a total refresh of the cache

OPTS1	= --maxdocs=50000 --maxthreads=1 --json-maxpkg=200
OPTS0	= $(OPTS1) --valid=$(validix)

jserv-schema	:
	$(MAKE) json-schema OPTS="$(OPTS) $(JSERV)"

jserv-index	:
	$(MAKE) json-index OPTS="$(OPTS) $(JSERV)"

jserv-pkg	:
	$(MAKE) json-pkg OPTS="$(OPTS) $(JSERV)"

jserv-rank	:
	$(MAKE) json-rank OPTS="$(OPTS) $(JSERV)"

jserv-update	:
	$(MAKE) json-update OPTS="$(OPTS) $(JSERV)"

jserv-update-all	:
	$(MAKE) json-update-all OPTS="$(OPTS) $(JSERV)"

whole-jserv	: $(indexer)
	@echo not done: "$(MAKE) update-cache      > log/update-cache.out      2>&1"
	for i in jserv-schema jserv-pkg jserv-rank jserv-index ;\
	do \
	  $(MAKE) $$i > log/$$i.out 2>&1 ; \
	done

json-schema	: $(indexer)
	@echo "create Hayoo! Schema for Hunt server"
	$< $(RUNOPTS) --json-create-schema $(OPTS)

json-schema-delete	: $(indexer)
	@echo "delete Hayoo! Schema for Hunt server"
	$< $(RUNOPTS) --json-delete-schema $(OPTS)

json-index	: $(indexer)
	$< $(RUNOPTS) --json-fct $(OPTS0) $(OPTS)

json-pkg	: $(indexer)
	$< $(RUNOPTS) --json-pkg $(OPTS0) $(OPTS)

json-rank	: $(indexer)
	$< $(RUNOPTS) --json-pkg-rank $(OPTS0) $(OPTS)

whole-json	: $(indexer)
	@echo not done: "$(MAKE) update-cache      > log/update-cache.out      2>&1"
	for i in json-schema json-pkg json-rank json-index ; \
	do \
	  $(MAKE) $$i > log/$$i.out 2>&1 ; \
	done

VALID	= 1month
json-all	: $(indexer)
	$< $(RUNOPTS) --json-all $(OPTS1) $(OPTS) --hackage --valid=$(VALID)


LATEST=1day
json-update	: $(indexer)
	$< $(RUNOPTS) --json-all $(OPTS1) $(OPTS) --hackage --latest=$(LATEST)


JSONFILES = $(shell find json -name '*.js' -print | sort)
insert-json:
	i=1; \
	for file in $(JSONFILES); do \
		echo ; echo -n "$$i - $$(date) - $$file "; \
		curl -X POST -d @"$$file" $(JHOST)/eval; \
		i=$$(($$i+1)); \
	done ; \
        curl $(JHOST)/binary/save/hayoo-ix.$$(date +%FT%T)

MAXJS	   = 10
JSONFILES1 = $(shell find json -name '*.js' -print | sort | head -$(MAXJS))
insert-json1:
	i=1; \
	for file in $(JSONFILES1); do \
		echo ; echo -n "$$i - $$(date) - $$file "; \
		curl -X POST -d @"$$file" $(JHOST)/eval; \
                echo curl $(JHOST)/binary/save/hayoo-ix.$$(date +%FT%T); \
                curl $(JHOST)/binary/save/hayoo-ix.$$(date +%FT%T); \
		i=$$(($$i+1)); \
	done ; \

#----------------------------------------

update-cache	: $(indexer)
	@echo update the hayoo cache with all packages uploaded to hackage within the last $(latest)
	@echo the list of loaded pages is written into file "cache.xml"
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< $(RUNOPTS) --cache --hackage --maxthreads=$(threads) --maxpar=$(maxpar) --latest=$(latest) --xml-output=cache.xml

packages	= hxt,hxt-unicode

load-cache	: $(indexer)
	@echo load the following list of packages into local cache: $(packages)
	@echo use "make load-cache packages=pack1,pack2,pack3" to specify the packages 
	[ -d ./tmp ]   || mkdir tmp
	[ -d ./cache ] || mkdir cache
	$< +RTS -N$(N) -s -K100M -RTS --cache --valid=$(valid) --packages=$(packages) --xml-output=-

clean	:
	rm -f *.o *.hi $(progs) *.out

reset	:
	rm -rf cache/* tmp/*

pkglist = hxt-filter,hxt,hxt-http

testcache	:
	$(indexer) --cache --maxthreads=1 --maxpar=20 --maxdocs=2500 \
	 --valid=1month -p $(pkglist)

testix	:
	$(indexer) --json-fct --maxthreads=1 --maxpar=20 --maxdocs=2500 \
	 --valid=1month -p $(pkglist)

testpkg	:
	$(indexer) --json-pkg --maxthreads=0 --maxpar=250 --maxdocs=2500 \
	 --valid=1month -p $(pkglist)

testrank	:
	$(indexer) --json-pkg-rank --maxthreads=0 --maxpar=250 --maxdocs=2500 \
	 --valid=1month -p $(pkglist)

test	: testix testpkg testrank testcache

.PHONY	: all force \
	whole-cache \
	clean reset \
	test testpkg testix testrank testcache
