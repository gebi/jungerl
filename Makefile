all: conf
	(cd lib; $(MAKE) $@)

clean:
	(cd lib; $(MAKE) $@)

$(LIBS): conf $(MK_INCLUDE)
# Build a program, unless it has a file called SKIP in its top directory
	if [ ! -e lib/$@/SKIP ]; then (cd lib/$@; $(MAKE) all); fi

$(MK_INCLUDE): $(MK_INCLUDE).in
	$(MAKE) conf

conf:
	(cd config; $(MAKE))
	(cd lib; $(MAKE) $@)

conf_clean:
	(cd config; $(MAKE) clean)

config/configure: config/configure.in
	(cd config; autoconf)

