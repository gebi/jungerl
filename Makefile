
LIBS = tuntap lersp

LIB_DIRS = $(LIBS:%=lib/%)

all: $(LIBS)

clean:

	@for dir in $(LIB_DIRS); do (cd $$dir; $(MAKE) clean); done

$(LIBS): conf $(MK_INCLUDE)
	(cd lib/$@; $(MAKE) all)

$(MK_INCLUDE): $(MK_INCLUDE).in
	$(MAKE) conf

conf:
	(cd config; make)

conf_clean:
	(cd config; make clean)

config/configure: config/configure.in
	(cd config; autoconf)

