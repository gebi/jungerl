
ifeq ($(TYPE),debug)
EXTRA_FLAGS = -g
else
EXTRA_FLAGS = 
endif

include  ../../../support/include.mk
PIC=-fpic

ERL = $(ERLDIR)/usr/include
# driver.h was moved into obsolete/ - let's be lazy and keep using it
# for now. :-)
ERL_OBS = $(ERL)/obsolete

all:    ../priv/slang_drv.so


slang_drv.o: slang_drv.c
	$(CC) -o slang_drv.o -c $(PIC) $(EXTRA_FLAGS)  -I$(ERL) -I $(ERL_OBS) \
	-I$(SLANG_INCLUDE) -I/usr/local/include slang_drv.c 

../priv/slang_drv.so:	slang_drv.o
	mkdir -p ../priv
	$(LD_SHARED) -o ../priv/slang_drv.so slang_drv.o \
	-L/usr/local/lib -lslang

clean:
	-rm -f *.o ../priv/*.so *~ 2> /dev/null
