

all:
	(cd src;$(MAKE))
	-(cd c_src;$(MAKE) -k)

clean:
	(cd src;$(MAKE) clean)
	(cd c_src;$(MAKE) clean)

release: clean
	sh ../../support/create_release.sh

