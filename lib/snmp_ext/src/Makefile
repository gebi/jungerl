MODULES = \
	snmp_agent_adapter \
	snmp_agentx \
	snmp_smux \
	snmp_smux_admin \
	snmp_smux_server \
	SMUX \
	RFC1155-SMI

ERL_FILES = $(MODULES:%=%.erl)

HRLS =  SMUX.hrl

HRL_FILES = $(HRLS:%=%.hrl)

TARGET_FILES = $(MODULES:%=../ebin/%.beam)

all: $(TARGET_FILES)

SMUX.erl: SMUX.asn RFC1155-SMI.asn1db
	erlc -bber_bin +optimize $<

SMUX.hrl: SMUX.asn RFC1155-SMI.asn1db
	erlc -bber_bin +optimize $<

RFC1155-SMI.asn1db: RFC1155-SMI.asn
	erlc -bber_bin +optimize $<

RFC1155-SMI.erl: RFC1155-SMI.asn
	erlc -bber_bin +optimize $<

../ebin/%.beam: %.erl $(HRLS)
	erlc $(ERLC_FLAGS) -o ../ebin $<

clean:
	rm -f $(TARGET_FILES)
	rm -f SMUX.hrl
	rm -f SMUX.erl
	rm -f SMUX.asn1db
	rm -f SMUX.beam
	rm -f RFC1155-SMI.asn1db
	rm -f RFC1155-SMI.erl
	rm -f RFC1155-SMI.hrl
	rm -f RFC1155-SMI.beam
