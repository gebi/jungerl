
##ERLC=/opt/R9C/bin/erlc
ERLC=erlc
ERLC_FLAGS=-W
EMULATOR=beam

../ebin/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<
#	$(ERLC) -b $(EMULATOR) $(ERLC_FLAGS) -o ../ebin $<

EFILES = esmb.erl esmb_client.erl

EBIN = ../ebin
EBIN_FILES = $(EFILES:%.erl=$(EBIN)/%.$(EMULATOR))

all: $(EBIN_FILES)

clean:
	rm -rf $(EBIN_FILES)
