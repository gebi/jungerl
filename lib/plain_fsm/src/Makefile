include ../../../support/include.mk

ERLC_FLAGS += -pa ../ebin -I ../inc

all: ../ebin/plain_fsm_xform.beam \
     ../ebin/plain_fsm.beam \
     ../ebin/fsm_example.beam

clean:
	-rm -f $(ERL_OBJECTS)

