include ../../../support/include.mk

ERLS  = $(shell echo *.erl)
FILES =	$(patsubst %.erl, %, $(ERLS))
DEPS = $(patsubst %, ../ebin/%.beam, panHost panTarg panScan panLib)

default: all
all: $(FILES)

$(FILES): % : ../ebin/%.beam

$(DEPS): ../ebin/%.beam : %.erl ../inc/pan.hrl
	$(ERLC) -o ../ebin -I ../inc $(ERLC_FLAGS) $<
../ebin/%.beam:		  %.erl
	$(ERLC) -o ../ebin $(ERLC_FLAGS) $<
