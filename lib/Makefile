LIBS := \
	builder \
	ce \
	distel \
	edoc \
	eldap \
	enfs \
	erl_cgi \
	ermacs \
	ex11 \
	gd1_drv \
        gen_leader \
	slang \
	ssh \
	lersp \
        lines \
	measurement \
	msc \
	plain_fsm \
        pan \
        pico \
	posix_drv \
	proc \
	proc_reg \
	quickcheck \
	rdbms \
	rpc \
	smart_exceptions \
	spread_drv \
        syntax_tools \
	tuntap \
	ucs \
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
gen_leader: edoc
lines: edoc
measurement: edoc
plain_fsm: edoc
wiki: pico
wumpus: edoc
xmlrpc: xmerl
xmerl: ucs


.PHONY: $(LIBS)

