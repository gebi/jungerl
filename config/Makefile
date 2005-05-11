MK_INCLUDE=../support/include.mk
JERL=../bin/jerl

all: config.status $(MK_INCLUDE) $(JERL)

config.status: configure
	./configure
	chmod +x $(JERL)

$(JERL) $(MK_INCLUDE): config.status ../support/include.mk.in ../bin/jerl.in
	./config.status
	chmod +x $(JERL)

configure: configure.in
	autoheader
	autoconf

clean:
	-rm -f config.cache config.log config.status configure \
               $(MK_INCLUDE) $(JERL)

