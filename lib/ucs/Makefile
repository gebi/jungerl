# ifeq "${MA_TOP}" ""
# MA_TOP = ../..
# endif

# -include $(MA_TOP)/config/make.subdir

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include vsn.mk

SUB_DIRECTORIES = src priv

# SPECIAL_TARGETS = 

all build opt:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE)); \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) clean); \
	done

test:
	cd priv; $(MAKE) test

# Release as in sourceforge src releases
BASE_REL = /var/tmp/ucs-$(UCS_VSN)
OTHER_FILES = changes.txt Makefile vsn.mk ucs.pub ucs.doc
release:
	mkdir $(BASE_REL)
	cp -p $(OTHER_FILES) $(BASE_REL)
	mkdir $(BASE_REL)/ebin
	mkdir $(BASE_REL)/include
	cp -p include/unidata.hrl $(BASE_REL)/include
	mkdir $(BASE_REL)/priv
	mkdir $(BASE_REL)/priv/db
	mkdir $(BASE_REL)/priv/testdata
	cp -p priv/Makefile $(BASE_REL)/priv/
	cp -p priv/testdata/*.txt $(BASE_REL)/priv/testdata
	cp -p priv/UnicodeData.txt priv/character-sets \
		priv/character-sets-not-IANA priv/ucsmnem.edat  $(BASE_REL)/priv
	mkdir $(BASE_REL)/src
	cd src; make RELSYSDIR=$(BASE_REL) release_src
	cd $(BASE_REL)/..; tar -czvf ucs.tar.gz ucs-$(UCS_VSN)
	mv $(BASE_REL)/../ucs.tar.gz .
	rm -rf $(BASE_REL)
