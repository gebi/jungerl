#
# Make the loop_drv (loop back driver)
#
include ../../../support/include.mk

DLL=so
OBJ=o
OUT=-o 

override CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I../../../support

debug: CFLAGS += -DDEBUG

LOOP_DRV = ../priv/loop_drv.$(DLL)

all: $(LOOP_DRV)

debug: all

depend:

$(LOOP_DRV): loop_drv.$(OBJ)
	$(LD_SHARED) $(OUT)$@ loop_drv.$(OBJ) $(DLL_LIBS)

clean :
	-rm $(LOOP_DRV) loop_drv.$(OBJ)
