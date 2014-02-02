

SERVER  = http://localhost:3000

json:
	$(MAKE) -C hayoo-json all

insert: 
	$(MAKE) -C hayoo-json insert SERVER=$(SERVER)

.PHONY: all clean json

all: 

clean:

