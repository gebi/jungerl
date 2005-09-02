ERLC = $(OTP_ROOT)/bin/erlc
ERLS = $(wildcard *.erl)
BEAMS = $(patsubst %.erl, %.beam, $(ERLS))

#.PHONY: default all 

default: all
all: $(BEAMS)

clean:
	rm -f *.beam

%.beam : %.erl
	$(ERLC) $<
