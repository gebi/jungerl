include ../../../support/include.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I.

UNIXDOM_DRV_SO = ../priv/unixdom_drv.so

SHLIB_OBJS = unixdom_drv.o my-unixdom.o erl_driver_tk.o

all: $(UNIXDOM_DRV_SO)

$(UNIXDOM_DRV_SO): $(SHLIB_OBJS)
	$(LD_SHARED) -o $@ $^
	# Symlink is only to help when using "jerl" script.
	-ln -s $(UNIXDOM_DRV_SO) ../ebin

unixdom_drv.o: unixdom_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

my-unixdom.o: my-unixdom.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

erl_driver_tk.o: erl_driver_tk.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm $(UNIXDOM_DRV_SO) ../ebin/*.so

.INTERMEDIATE: $(SHLIB_OBJS)

