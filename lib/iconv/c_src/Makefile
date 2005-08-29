include ../../../support/include.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I../../../support -I.

ICONV_DRV_SO = ../priv/iconv_drv.so

ICONV_SHLIB_OBJS = iconv_drv.o

all: $(ICONV_DRV_SO)

$(ICONV_DRV_SO): $(ICONV_SHLIB_OBJS)
	$(LD_SHARED) -o $@ $^

iconv_drv.o: iconv_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm -f $(ICONV_DRV_SO) ../ebin/*.so *.o

.INTERMEDIATE: $(ICONV_SHLIB_OBJS)
