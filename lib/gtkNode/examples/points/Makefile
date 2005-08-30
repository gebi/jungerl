ERLC = erlc
ERLS = $(wildcard *.erl)
BEAMS = $(patsubst %.erl, %.beam, $(ERLS))

#.PHONY: default all 

default: all
all: $(BEAMS)

clean:
	rm *.beam

%.beam : %.erl
	$(ERLC) $<
