include ../../../support/include.mk

CFLAGS += -I $(ERL_C_INCLUDE_DIR)

PSOCKET_SRV = ../priv/psocket_srv

all: $(PSOCKET_SRV)

$(PSOCKET_SRV): psocket_srv.c
	$(CC) -o $@ $<

