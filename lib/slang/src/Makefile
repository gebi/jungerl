include ../../../support/include.mk
include ../vsn.mk

ERLC_FLAGS += -I../include

TARGETS = $(ERL_OBJECTS) ../ebin/slang.app

all debug: $(TARGETS)

clean:
	rm -f $(TARGETS)

conf: # nothing

../ebin/slang.app:	slang.app.src
	cat slang.app.src | sed 's/%VSN%/${SLANG_VSN}/' > ../ebin/slang.app

$(ERL_OBJECTS): $(ERL_HEADERS)

