include ../../../support/include.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I.

POSIX_DRV_SO = ../priv/posix_drv.so

SHLIB_OBJS = posix_drv.o my-posix.o erl_driver_tk.o

all: $(POSIX_DRV_SO)

$(POSIX_DRV_SO): $(SHLIB_OBJS)
	ld -G -o $@ $^
	# Symlink is only to help when using "jerl" script.
	-ln -s $(POSIX_DRV_SO) ../ebin

posix_drv.o: posix_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

my-posix.o: my-posix.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

erl_driver_tk.o: erl_driver_tk.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm $(POSIX_DRV_SO) ../ebin/*.so

.INTERMEDIATE: $(SHLIB_OBJS)

