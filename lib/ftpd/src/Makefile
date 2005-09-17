include ../../../support/include.mk

include ../vsn.mk
VSN=$(FTPD_VSN)


MODULES = ftpd \
	  ftp_test


EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(EBIN)/ftpd.app


all: $(EBIN_FILES)

debug: 
	$(MAKE) ERLC_FLAGS+="$(ERLC_FLAGS) +debug_info -Ddebug"

clean:
	-rm $(ERL_OBJECTS)

$(EBIN)/ftpd.beam: ftpd.hrl
$(EBIN)/ftp_test.beam: ftpd.hrl

