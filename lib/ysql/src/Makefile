
include ../../../support/include.mk

include ../vsn.mk
VSN=$(YSQL_VSN)

DEBUG=

DOCDIR=`pwd`

ERL=erl
ERLC=erlc
ERLC_FLAGS+=-W $(DEBUG)
EMULATOR=beam


MODULES = ysql

EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(EBIN)/ysql.app

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES)

$(EBIN)/ysql.beam: ../include/ysql.hrl

