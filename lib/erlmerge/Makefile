
all: conf
	$(MAKE) -f Makefile.top

clean:
	$(MAKE) -f Makefile.top clean

conf:
	(cd config; $(MAKE))

conf_clean:
	(cd config; $(MAKE) clean)

install:
	$(MAKE) -f Makefile.top install

release: clean appfile
	sh ../../support/create_release.sh

appfile:
	(cd src;$(MAKE) ../ebin/erlmerge.app)
