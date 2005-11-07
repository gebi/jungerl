.PHONY: default all clean realclean

default: all

all: 
	$(MAKE) -k -C src
	$(MAKE) -k -C examples

realclean: clean

clean: 
	rm -f src/deps.mk
	rm -f examples/*/*.beam
	rm -f ebin/*.beam
	rm -f priv/obj/*.o
