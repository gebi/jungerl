include ../../../support/include.mk

# Some modules are automatically generated, so we won't use the
# include makefile's auto-detected list.
MODULES := spread_test
OBJECTS := $(MODULES:%=../ebin/%.$(EMULATOR))

ERLC_FLAGS += -I../src

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS)
	rm -f core core.* *.core erl_crash.dump ktrace.out
	rm -f TEST?
	rm -f test*.*

test: all
	erl +A5 -pz ../ebin -pz ../priv -s spread_test regression -s erlang halt

