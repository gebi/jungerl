SUBDIRS = $(wildcard [a-z]*)

.PHONY: $(SUBDIRS) default

default: $(SUBDIRS)

clean: 
	-rm -f */*.beam

$(SUBDIRS): 
	$(MAKE) -k -C $@
