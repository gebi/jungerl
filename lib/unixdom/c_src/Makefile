include ../../../support/include.mk

APPS :=	../priv/unixdom_echosrv ../priv/unixdom_cat ../priv/tcp_echoserv
LIBS :=	../priv/unixdom_drv.so ../priv/exp1_drv.so

CFLAGS += -I $(ERL_C_INCLUDE_DIR) -I $(ERL_C_INCLUDE_DIR)/obsolete
CFLAGS += -g -DNEED_EXTRA_FUNCS -Wall -pedantic

TARGET_FILES := $(APPS) $(LIBS)

all: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES)
	rm -f *.o *.so core *.core ktrace.out

../priv/unixdom_echosrv: unixdom_echosrv.c
	$(CC) $(CFLAGS) -o unixdom_echosrv unixdom_echosrv.c `./Is-Solaris-link.sh`

../priv/unixdom_cat: unixdom_cat.c
	$(CC) $(CFLAGS) -o unixdom_cat unixdom_cat.c `./Is-Solaris-link.sh`

../priv/tcp_echoserv: tcp_echoserv.c
	$(CC) $(CFLAGS) -o tcp_echoserv tcp_echoserv.c `./Is-Solaris-link.sh`

../priv/%.so: %.o
	ld -G -o $@ $<

