include ../../../support/include.mk

ERLC_FLAGS += -I ../include

all: $(ERL_OBJECTS)

clean:
	-rm $(ERL_OBJECTS) 

eradius_dict.beam: eradius_dict.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<
