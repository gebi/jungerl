LIBS := \
	ce \
	edoc \
	enfs \
	ermacs \
	ex11 \
	slang \
	lersp \
	msc \
        pan \
	rpc \
        syntax_tools \
	tuntap \
        wumpus \
        www_tools \
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
ce: edoc
edoc: syntax_tools xmerl
ermacs: slang msc
enfs: rpc
wumpus: edoc
xmlrpc: xmerl


.PHONY: $(LIBS)

