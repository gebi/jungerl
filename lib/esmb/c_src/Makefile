include ../../../support/include.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I.

MD4_DRV_SO = ../priv/md4_drv.so

SHLIB_OBJS = md4_drv.o md4.o

all: $(MD4_DRV_SO)

$(MD4_DRV_SO): $(SHLIB_OBJS)
	ld -G -o $@ $^
	# Symlink is only to help when using "jerl" script.
	-ln -s $(MD4_DRV_SO) ../ebin

test: test.o md4.o
	$(CC) -o md4test test.o md4.o

md4_drv.o: md4_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

md4.o: md4.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

test.o: test.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm -f $(MD4_DRV_SO) ../ebin/*.so

.INTERMEDIATE: $(SHLIB_OBJS)
