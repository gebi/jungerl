include ../../../support/include.mk

ERLC_FLAGS += -I ../include

.SUFFIXES: .asn .erl .beam


all: asn $(ERL_OBJECTS)

asn: ELDAPv3.erl ../ebin/ELDAPv3.beam

ELDAPv3.erl: ELDAPv3.asn
	${ERLC} ELDAPv3.asn
	$(ERLC) -o$(EBIN) ELDAPv3.erl

clean:
	-rm $(ERL_OBJECTS) ELDAPv3.erl ELDAPv3.beam ELDAPv3.asn1db ELDAPv3.hrl


