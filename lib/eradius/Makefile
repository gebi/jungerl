
all:
	(cd src;$(MAKE) eradius_dict.beam)
	(cd priv;$(MAKE))
	(cd src;$(MAKE))
	(cd test;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd priv;$(MAKE) clean)
	(cd test;$(MAKE) clean)

