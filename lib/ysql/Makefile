

all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd priv/docroot;$(MAKE) clean)

release: clean appfile
	sh ../../support/create_release.sh

appfile:
	(cd src;$(MAKE) ../ebin/ysql.app)


# Target for the gettext handling
gettext:
	rm -f ebin/*.beam
	(cd src; $(MAKE))
