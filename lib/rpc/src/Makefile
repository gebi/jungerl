# Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
# This Makefile may require GNU make.

include ../vsn.mk

# Override these as necessary for your Erlang environment.
#KERNEL_VSN=2.6.1.2.1
#STDLIB_VSN=1.9.1.1
#ERTS_VSN=5.0.1.1.b3
KERNEL_VSN=2.15.1
STDLIB_VSN=1.18.1
ERTS_VSN=5.9.1
EMULATOR = beam
ERLC = erlc
ERL = erl
#

EBIN=../ebin
ESRC=.
ERPCGEN = ../priv/erpcgen
ERPCGEN_OPTS =

ERPCGEN_MODULES = erpcgen xdr_parse xdrgen xdr_scan

RPC_MODULES = rpc_client rpc_xdr rpc_server \
	      pmap pmap_clnt pmap_xdr \
	      xdr_auth

MODULES =  $(RPC_MODULES) $(ERPCGEN_MODULES)

# This is a Perl script for producing .app files from a template
#
APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

ERPCGEN_TARGET_FILES= $(ERPCGEN_MODULES:%=../ebin/%.$(EMULATOR))
RPC_TARGET_FILES= $(RPC_MODULES:%=../ebin/%.$(EMULATOR))
TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(ERPCGEN)

%_xdr.erl %.hrl: %.x $(ERPCGEN)
	$(ERPCGEN) -a '[xdrlib]' $*

%_clnt.erl: %.x $(ERPCGEN)
	$(ERPCGEN) -a '[clnt]' $*

$(EBIN)/%.$(EMULATOR): $(ESRC)/%.erl
	$(ERLC) -b$(EMULATOR) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

all: $(TARGET_FILES) erpcgen.boot erpcgen.script ../ebin/rpc.app

$(ERPCGEN): erpcgen.boot erpcgen.script
	sed -e 's;%ERL%;$(ERL);' \
	  erpcgen.src > $(ERPCGEN)
	chmod u+x $(ERPCGEN)

erpcgen.boot erpcgen.script: ../ebin/erpcgen.app erpcgen.rel $(ERPCGEN_TARGET_FILES)
	$(ERLC) -pa ../ebin erpcgen.rel

erpcgen.rel:	erpcgen.rel.src
	sed 	-e 's;%ERTS_VSN%;$(ERTS_VSN);' \
		-e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
		-e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
		-e 's;%ERPCGEN_VSN%;$(ERPCGEN_VSN);' \
	erpcgen.rel.src > erpcgen.rel

../ebin/erpcgen.app:	erpcgen.app.src
	perl -e $(APPSCRIPT) "$(ERPCGEN_VSN)" $(ERPCGEN_MODULES) < $< > $@

../ebin/rpc.app:	rpc.app.src
	perl -e $(APPSCRIPT) "$(RPC_VSN)" $(RPC_MODULES) < $< > $@

xdr_parse.erl: xdr.yrl
	$(ERLC) -o xdr_parse.erl xdr.yrl

clean:
	-rm -f $(TARGET_FILES)
	-rm -f core erl_crash.dump
	-rm -f pmap_clnt.erl pmap_xdr.erl pmap.hrl
	-rm -f rpc_xdr.erl rpc.hrl
	-rm -f erpcgen.boot erpcgen.script xdr_parse.erl
	-rm -f ../ebin/erpcgen.app ../ebin/rpc.app erpcgen.rel


../ebin/pmap.beam: pmap.hrl
../ebin/pmap_clnt.beam: pmap.hrl
../ebin/pmap_svc.beam: pmap.hrl
../ebin/rpc_client.beam: rpc.hrl
../ebin/rpc_server.beam: rpc.hrl
rpc_xdr.erl: rpc.x
pmap_xdr.erl: pmap.x
pmap_clnt.erl: pmap.x

