SUBDIRS = $(wildcard [a-z]*)

.PHONY: $(SUBDIRS) default

default: $(SUBDIRS)

clean: 
	rm */*.beam

$(SUBDIRS): 
	$(MAKE) -k -C $@
