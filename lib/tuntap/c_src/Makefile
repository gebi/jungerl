include ../../../support/include.mk

CFLAGS += -I $(ERL_C_INCLUDE_DIR)

TUN_DRV_SO = ../priv/tun_drv.so

all: $(TUN_DRV_SO)

$(TUN_DRV_SO): tun_drv.o
	ld -G -o $@ $<

tun_drv.o: tun_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm ../priv/tun_drv.so

config: # nothing

.INTERMEDIATE: tun_drv.o

