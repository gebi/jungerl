include ../../../support/include.mk

CFLAGS += -I${ERL_DIR}/usr/include

all: passwd_checker

# Requires cracklib to be installed!
passwd_checker: passwd_checker.o
	$(LINK.c) -o ../priv/$@ $< -lcrack

clean:
	-rm -f ../priv/passwd_checker *.o

