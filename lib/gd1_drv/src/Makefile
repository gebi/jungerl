include ../../../support/include.mk

# Some modules are automatically generated, so we won't use the
# include makefile's auto-detected list.
MODULES := gd1_drv
OBJECTS := $(MODULES:%=../ebin/%.$(EMULATOR))

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS)

