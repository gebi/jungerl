include ../../../support/include.mk

all: $(ERL_OBJECTS)

debug: 
	$(MAKE) ERLC_FLAGS+="$(ERLC_FLAGS) +debug_info -Ddebug"

clean:
	-rm $(ERL_OBJECTS)
