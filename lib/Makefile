LIBS := \
	enfs \
	ermacs \
	slang \
	lersp \
	msc \
	rpc \
	tuntap \
	xmerl \
	xmlrpc

all: $(LIBS)

clean:
	@for dir in $(LIBS); do \
	  if [ ! -e $@/SKIP ]; then \
	    (cd $$dir; $(MAKE) $@) \
	  fi \
	done

$(LIBS):
	@if [ ! -e $@/SKIP ]; then \
	  (cd $@; $(MAKE) all) \
	fi

# Dependencies between programs.
ermacs: slang msc
enfs: rpc
xmlrpc: xmerl


.PHONY: $(LIBS)

