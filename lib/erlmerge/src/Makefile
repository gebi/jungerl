
include ../config/include.mk


EBIN = ../ebin

EFILES = erlmerge.erl

EBIN_FILES = $(EFILES:%.erl=$(EBIN)/%.$(EMULATOR))

all: $(EBIN_FILES)

clean:
	rm -rf $(EBIN_FILES)

