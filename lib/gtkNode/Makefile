.PHONY: default all clean realclean generate

default: all

all: 
	$(MAKE) -C src
	$(MAKE) -C examples

clean: 
	$(MAKE) -C src clean
	$(MAKE) -C examples clean

realclean: clean
	$(MAKE) -C priv/generator clean

generate:
	$(MAKE) -C priv/generator
