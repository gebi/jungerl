include ../../../support/include.mk

MODULES = ex2 ex1 
TARGETS = $(MODULES:%=%.beam) 

all debug:	$(TARGETS)

clean:
	rm -f $(TARGETS)

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
