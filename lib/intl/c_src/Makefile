include ../../../support/include.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -DLOADABLE -fpic

INTL_DRV_SO = ../priv/intl_drv.so

all: $(INTL_DRV_SO)

$(INTL_DRV_SO): intl_drv.o
	$(LD_SHARED) -o$@ intl_drv.o $(DLL_LIBS)

clean:
	-rm -f $(INTL_DRV_SO) intl_drv.o

