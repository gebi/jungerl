#include ../../../support/include.mk
ERLC = erlc
ERLC_FLAGS = -W +debug_info
EBIN = ../ebin
INC = ../inc

ERLS  = $(wildcard *.erl)
FILES =	$(patsubst %.erl, %, $(ERLS))
DEPS = $(patsubst %, $(EBIN)/%.beam, panHost panTarg panScan panLib)

default: all
all: $(FILES)

$(FILES): % : $(EBIN)/%.beam

$(DEPS): $(EBIN)/%.beam : %.erl $(INC)/pan.hrl
	$(ERLC) -o $(EBIN) -I $(INC) $(ERLC_FLAGS) $<
$(EBIN)/%.beam:		  %.erl
	$(ERLC) -o $(EBIN) $(ERLC_FLAGS) $<
