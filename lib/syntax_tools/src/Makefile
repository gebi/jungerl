
EMULATOR = beam
EBIN = ../ebin

ERL_COMPILE_FLAGS += +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES=erl_syntax.erl erl_prettypr.erl erl_syntax_lib.erl	\
	erl_comment_scan.erl erl_recomment.erl erl_tidy.erl	\
	epp_dodger.erl prettypr.erl igor.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
