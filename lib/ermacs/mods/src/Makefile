include ../../../../support/include.mk

# Ermacs includes
ERLC_FLAGS += -I ../../.. -pa ../../ebin

all: $(ERL_OBJECTS)

$(ERL_OBJECTS): ../../include/edit.hrl

clean:
	-rm -f $(ERL_OBJECTS)

