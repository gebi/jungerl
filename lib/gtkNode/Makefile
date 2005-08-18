default: all

all: 
	$(MAKE) -k -C src
	$(MAKE) -k -C examples

clean: 
	rm examples/*/*.beam
	rm ebin/*.beam
	rm priv/obj/*.o

generate:
	$(MAKE) -k -C priv/generator/src
