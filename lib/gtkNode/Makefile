.PHONY: default all clean generate tarfile

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

tarfile:
	tar -czf gtkNode.tgz -C .. -P \
	--exclude="*~" --exclude="CVS" --exclude=".cvs*" --exclude=gtkNode.tgz \
	gtkNode
