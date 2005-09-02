ERLC = erlc
ERL_FLAGS = +debug_info
ERLS = $(wildcard *.erl)
BEAMS = $(patsubst %.erl, %.beam, $(ERLS))

#.PHONY: default all 

default: all
all: $(BEAMS)

clean:
	rm *.beam

%.beam : %.erl
	$(ERLC) $(ERL_FLAGS) $<
