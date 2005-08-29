
include ../../../support/include.mk

include ../vsn.mk
VSN=$(ICONV_VSN)

DEBUG=

DOCDIR=`pwd`

ERL=erl
ERLC=erlc
ERLC_FLAGS+=-W $(DEBUG)
EMULATOR=beam


MODULES = iconv

EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(EBIN)/iconv.app

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES)

