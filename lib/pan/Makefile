include ../../support/include.mk

EFLAGS = -W +debug_info

ERLS  = $(shell echo src/*.erl)
BEAMS =	$(patsubst src/%.erl, ebin/%.beam, $(ERLS))



ebin/%.beam:	src/%.erl
	$(ERLC) -o ebin -I inc $(EFLAGS) $<

default: all
all: $(BEAMS)

clean: 
	rm $(BEAMS)
