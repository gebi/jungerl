EBIN=../ebin
INCLUDE=../inc
EMULATOR=beam
ERL_COMPILE_FLAGS += -I $(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import -pz $(EBIN)

SOURCES=  gen_leader.erl gdict.erl test_cb.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))


.SUFFIXES: .erl .$(EMULATOR)


all: $(OBJECTS)

clean:
	-rm -f $(OBJECTS)

distclean: clean

realclean: clean

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -W $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

