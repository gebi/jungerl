include ../../../support/include.mk

include ../../../support/SpreadConfig.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I. \
	$(LIBSPREAD_CPPFLAGS) \
	-DDRIVER_USING_PTHREADS -D_THREAD_SAFE -D_REENTRANT

SPREAD_DRV_SO = ../priv/spread_drv.so

SHLIB_OBJS = spread_drv.o my-spread.o erl_driver_tk.o

all: $(SPREAD_DRV_SO)

$(SPREAD_DRV_SO): $(SHLIB_OBJS)
	ld -G -o $@ $^ $(LIBSPREAD_LDFLAGS)
# Symlink is only to help when using "jerl" script.
	-ln -s $(SPREAD_DRV_SO) ../ebin

spread_drv.o: spread_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

my-spread.o: my-spread.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

erl_driver_tk.o: erl_driver_tk.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm $(SPREAD_DRV_SO) ../ebin/*.so

.INTERMEDIATE: $(SHLIB_OBJS)

