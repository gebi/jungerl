ERLC =	/EDUP/Erlang/R7B-4/bin/erlc
EFLAGS = -W +debug_info

ERLS  = $(shell echo *.erl)
BEAMS =	$(patsubst %.erl, %.beam, $(ERLS))

default:$(BEAMS)

%.beam:	%.erl
	$(ERLC) $(EFLAGS) $<
