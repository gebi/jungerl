#
# Make the sl (serial line driver)
#
include ../../../support/include.mk

DLL=so
OBJ=o
OUT=-o 

CFLAGS += -I$(ERL_C_INCLUDE_DIR)

SL_DRV = ../priv/sl_drv.$(DLL)

all: $(SL_DRV)

$(SL_DRV): sl_drv.$(OBJ)
	$(LD_SHARED) $(OUT)$@ sl_drv.$(OBJ) $(DLL_LIBS)

clean :
	-rm $(SL_DRV) sl_drv.$(OBJ)
