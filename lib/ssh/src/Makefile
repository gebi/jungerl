
include ../vsn.mk
VSN=$(SSH_VSN)

.SUFFIXES: .asn1
.PRECIOUS: %.erl



ASN_MODULES = PKCS-1 DSS
ASN_ASNS = $(ASN_MODULES:%=%.asn1)
ASN_ERLS = $(ASN_MODULES:%=%.erl)
ASN_HRLS = $(ASN_MODULES:%=%.hrl)
ASN_DBS = $(ASN_MODULES:%=%.asn1db)
ASN_TABLES = $(ASN_MODULES:%=%.table)


ASN_FLAGS = -bber_bin +der +compact_bit_string +optimize +noobj

APP_FILE= ssh.app
APP_SRC= $(APP_FILE).src
APP_TARGET= ../ebin/$(APP_FILE)

MODULES = \
	$(ASN_MODULES) \
	ssh_math \
	ssh_rsa \
	ssh_dsa \
	ssh_bits \
	ssh_file \
	ssh_io \
	ssh_proto \
	ssh_cm \
	ssh_userauth \
	ssh_xfer \
	ssh_tcp \
	ssh_tcp_wrap \
	ssh_dist \
	ssh_epmd \
	ssh \
	sftp


INCLUDE = ../include
EBIN = ../ebin


override ERLC_FLAGS += 

debug: ERLC_FLAGS += +debug_info -Ddebug -W

ERLC = erlc

OBJS = $(MODULES:%=$(EBIN)/%.beam)

TARGET_FILES = $(OBJS) $(APP_TARGET)

all:	$(TARGET_FILES) $(HRL_FILES)

debug:  all

depend:
	edep -MM -o $(EBIN) $(ERLC_FLAGS) $(MODULES:%=%.erl) > depend.mk

clean: 
	rm -f $(ASN_ERLS) $(GEN_ERLS) $(ASN_HRLS) $(HRL_FILES) $(ASN_DBS) \
		$(ASN_TABLES) $(TARGET_FILES)


$(HRL_FILES):	$(ASN_HRLS)
	cp -p $(ASN_HRLS) $(INCLUDE)

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

DSS.hrl:	DSS.erl
PKCS-1.hrl:	PKCS-1.erl


-include depend.mk

%.erl: %.asn1
	erlc $(ASN_FLAGS) $<



$(EBIN)/%.beam:	%.erl
	erlc -o $(EBIN) $(ERLC_FLAGS) $<








