
DEBUG=

##ERLC=/opt/R9C/bin/erlc
ERLC=erlc
ERLC_FLAGS=-W $(DEBUG)
EMULATOR=beam

../ebin/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<
#	$(ERLC) -b $(EMULATOR) $(ERLC_FLAGS) -o ../ebin $<

EFILES = esmb.erl esmb_client.erl md4.erl iconv.erl

EBIN = ../ebin
EBIN_FILES = $(EFILES:%.erl=$(EBIN)/%.$(EMULATOR))

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES)
