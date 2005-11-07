ERLC=$(OTP_ROOT)/bin/erlc
ERLC_FLAGS = -W +debug_info
EBIN = ../ebin
INC = ../include
MAKEDEPS = ./mk.deps
DEPS = deps.mk
COMPILE = $(ERLC) -o $(EBIN) -I $(INC) $(ERLC_FLAGS)

HRLS  = $(wildcard $(INC)/*.hrl)
ERLS  = $(wildcard *.erl)
FILES =	$(patsubst %.erl, %, $(ERLS))

default: $(DEPS) all
all: $(FILES)
clean:
	\rm -f $(EBIN)/*.beam

$(FILES): % : $(EBIN)/%.beam

$(DEPS): $(ERLS) $(HRLS) $(MAKEDEPS)
	$(MAKEDEPS) $(EBIN) $(INC) $@ COMPILE

include $(DEPS)
