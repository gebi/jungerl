
all: config.status

config.status: configure
	./configure

configure: configure.in
	autoconf

clean:
	-rm -f config.cache config.log config.status

