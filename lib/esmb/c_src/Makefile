
O_FILES = md4_drv.o md4.o 

DRIVER_H_DIR = /usr/local/lib/erlang/usr/include
FPIC = -fpic -shared
CFLAGS += -I$(DRIVER_H_DIR) $(FPIC)

OUTPUT = ../priv/md4_drv.so

all: $(O_FILES)
	$(CC) -o $(OUTPUT) $(CFLAGS) -DDYNAMIC_DRIVER $(O_FILES)

test: test.o md4.o
	$(CC) -o md4test test.o md4.o

clean:
	rm -f *.o md4test $(OUTPUT)
