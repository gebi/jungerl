include ../../../support/include.mk

include ../../../support/GDConfig.mk

CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I. \
	$(LIBGD_CPPFLAGS) $(LIBPNG_CPPFLAGS) $(LIBJPEG_CPPFLAGS) \
	$(LIBXPM_CPPFLAGS)

GD1_DRV_SO = ../priv/gd1_drv.so

SHLIB_OBJS = gd1_drv.o my-gd1.o erl_driver_tk.o

all: $(GD1_DRV_SO)

$(GD1_DRV_SO): $(SHLIB_OBJS)
	ld -G -o $@ $^ $(LIBGD_LDFLAGS) $(LIBPNG_LDFLAGS) \
		$(LIBJPEG_LDFLAGS) $(LIBXPM_LDFLAGS)
# Symlink is only to help when using "jerl" script.
	-ln -s $(GD1_DRV_SO) ../ebin

gd1_drv.o: gd1_drv.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

my-gd1.o: my-gd1.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

erl_driver_tk.o: erl_driver_tk.c
	$(CC) $(CFLAGS) -o $@ -c -fpic $(ERL_INCLUDE) $<

clean:
	-rm $(GD1_DRV_SO) ../ebin/*.so

.INTERMEDIATE: $(SHLIB_OBJS)

