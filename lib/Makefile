LIBS := \
	builder \
	ce \
	edoc \
	enfs \
	erl_cgi \
	ermacs \
	ex11 \
	gd1_drv \
	slang \
	lersp \
	measurement \
	msc \
	plain_fsm \
        pan \
        pico \
	posix_drv \
	quickcheck \
	rpc \
	smart_exceptions \
	spread_drv \
        syntax_tools \
	tuntap \
        wiki \
        wumpus \
        www_tools \
	xmerl \
	xmlrpc 

all: $(LIBS)

clean:
	@for dir in $(LIBS); do \
	  if [ ! -f $@/SKIP ]; then \
	    (cd $$dir; $(MAKE) $@) \
	  fi \
	done

$(LIBS):
	@if [ ! -f $@/SKIP ]; then \
	  (cd $@; $(MAKE) all) \
	fi

# Dependencies between programs.
ce: edoc
edoc: syntax_tools xmerl
ermacs: slang msc
enfs: rpc
measurement: edoc
wiki: pico
wumpus: edoc
xmlrpc: xmerl


.PHONY: $(LIBS)

