
include ../../../support/include.mk

include ../vsn.mk
VSN=$(FD_SERVER_VSN)

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

MODULES= fdsrv

EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(EBIN)/fd_server.app


all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES)

