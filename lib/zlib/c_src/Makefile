include ../../../support/include.mk

DLL=so
OBJ=o
OUT=-o 

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I. -DLOADABLE -fpic

ZLIB_DRV_SO = ../priv/zlib_drv.$(DLL)

all: $(ZLIB_DRV_SO)

$(ZLIB_DRV_SO): zlib_drv.$(OBJ)
	$(LD_SHARED) $(OUT)$@ zlib_drv.$(OBJ) $(DLL_LIBS) -lz

clean:
	-rm $(ZLIB_DRV_SO) zlib_drv.$(OBJ)


