include ../../../support/include.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I.

MD4_DRV_SO = ../priv/md4_drv.so
ICONV_DRV_SO = ../priv/iconv_drv.so

MD4_SHLIB_OBJS = md4_drv.o md4.o
ICONV_SHLIB_OBJS = iconv_drv.o

all: md4 iconv

md4: $(MD4_DRV_SO)

iconv: $(ICONV_DRV_SO)

$(MD4_DRV_SO): $(MD4_SHLIB_OBJS)
	ld -G -o $@ $^
	# Symlink is only to help when using "jerl" script.
	-ln -s $(MD4_DRV_SO) ../ebin

$(ICONV_DRV_SO): $(ICONV_SHLIB_OBJS)
	ld -G -o $@ $^
	# Symlink is only to help when using "jerl" script.
	-ln -s $(ICONV_DRV_SO) ../ebin

test: test.o md4.o
	$(CC) -o md4test test.o md4.o

md4_drv.o: md4_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

md4.o: md4.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

iconv_drv.o: iconv_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

test.o: test.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm -f $(ICONV_DRV_SO) $(MD4_DRV_SO) ../ebin/*.so

.INTERMEDIATE: $(MD4_SHLIB_OBJS) $(ICONV_SHLIB_OBJS)
