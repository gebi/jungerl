
include ../../../support/include.mk

include ../vsn.mk
VSN=$(YFRONT_VSN)

YAWS_INC_DIR=/usr/local/lib/yaws/include

DEBUG=

DOCDIR=`pwd`

ERL=erl
ERLC=erlc
ERLC_FLAGS+=-W $(DEBUG) -I $(YAWS_INC_DIR)
EMULATOR=beam


MODULES = yfront wblog ymnesia wraplog ehtml javascript passwd_checker

EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(EBIN)/yfront.app

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES)



