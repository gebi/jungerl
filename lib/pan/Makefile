include ../../support/include.mk



ERLS  = $(shell echo src/*.erl)
BEAMS =	$(patsubst src/%.erl, ebin/%.beam, $(ERLS))



ebin/%.beam:	src/%.erl
	$(ERLC) -o ebin -I inc $(ERLC_FLAGS) $<

default: all
all: $(BEAMS)

clean: 
	rm $(BEAMS)
