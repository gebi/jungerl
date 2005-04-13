
include ../../../support/include.mk

include ../vsn.mk
VSN=$(ESMB_VSN)

DEBUG=

ERL=erl
ERLC=erlc
ERLC_FLAGS=-W $(DEBUG)
EMULATOR=beam


MODULES = esmb \
	  esmb_client \
	  esmb_rpc \
	  md4 \
	  iconv

EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(EBIN)/esmb.app

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES)

## Need R10 (?)
doc: 
	$(MAKE) DOCDIR=`pwd` $(DOCDIR)/esmb_rpc.html

$(DOCDIR)/%.html:%.erl
	$(ERL) -noshell -run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}, {preprocess, true}]' -s init stop
