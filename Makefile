
LIBS = tuntap lersp

LIB_DIRS = $(LIBS:%=lib/%)

all: $(LIBS)

clean:
	@for dir in $(LIB_DIRS); do (cd $$dir; $(MAKE) clean); done

$(LIBS): conf $(MK_INCLUDE)
# Build a program, unless it has a file called SKIP in its top directory
	if [ ! -e lib/$@/SKIP ]; then (cd lib/$@; $(MAKE) all); fi

$(MK_INCLUDE): $(MK_INCLUDE).in
	$(MAKE) conf

conf:
	(cd config; make)

conf_clean:
	(cd config; make clean)

config/configure: config/configure.in
	(cd config; autoconf)

