include ../../../support/include.mk

# Some modules are automatically generated, so we won't use the
# include makefile's auto-detected list.
MODULES := unixdom_drv
OBJECTS := $(MODULES:%=../ebin/%.$(EMULATOR))

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS)
	rm -f ../ebin/unixdom_drv.so
	rm -f ../priv/unixdom_drv.so

