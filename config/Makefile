MK_INCLUDE=../support/include.mk

all: config.status $(MK_INCLUDE)

config.status: configure
	./configure

$(MK_INCLUDE): config.status
	./config.status

configure: configure.in
	autoconf

clean:
	-rm -f config.cache config.log config.status configure \
               $(MK_INCLUDE)

