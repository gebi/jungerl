include ../../support/include.mk

ERLS  = $(shell echo src/*.erl)
BEAMS =	$(patsubst src/%.erl, ebin/%.beam, $(ERLS))

default: all
all: $(BEAMS)

ebin/%.beam:	src/%.erl
	$(ERLC) -o ebin -I inc $(ERLC_FLAGS) $<

clean: 
	rm $(BEAMS)
