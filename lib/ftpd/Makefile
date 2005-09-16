

all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	-rm ./ebin/ftpd.app 

release: clean appfile
	sh ../../support/create_release.sh

appfile:
	(cd src;$(MAKE) ../ebin/ftpd.app)

