### --------------------------------------------------------------------
### Created: 15 Feb 1999 by Tobbe tobbe@cslab.ericsson.se
###
### $Id$
### --------------------------------------------------------------------

# Change path if not the default compiler shall be used
ERLC = erlc
CVS_CHECKED_IN=/export/home/tobbe/bin/cvs_checked_in

MODNAME    = ex11
ERL_FILES  = ex11.erl ex11_client.erl ex11_proto.erl ex11_utils.erl ex11_xauth.erl \
             ex11_db.erl rcube.erl matrix44.erl
JAM_FILES  = $(ERL_FILES:.erl=.jam)
BEAM_FILES = $(ERL_FILES:.erl=.beam)

RELEASE_FILES = Makefile $(ERL_FILES) ex11.hrl ex11.pub README README.developers


.SUFFIXES: .3 .html .sgml .jam .beam .hrl .erl

.erl.jam:
	-$(ERLC) -W -b jam $<

.erl.beam:
	-$(ERLC) -W -b beam $< 

all: jam beam

jam: $(JAM_FILES)
	@echo " *** Compilation finished ***"

beam: $(BEAM_FILES)
	@echo " *** Compilation finished ***"

sgml: 
	-sgml_transform -m 3 m.sgml
	-sgml_transform -html m.sgml
	@echo " *** SGML processing finished ***"

clean:
	rm -f *.jam *.beam *sgmls*

$(ERL_FILES): ex11.hrl

release:
	$(CVS_CHECKED_IN) .. $(MODNAME)
	@echo "*** Specify a release tag name (M-N), e.g 2-5 : "; read tag; \
	$(MAKE) do_release "FREEZE_TAG=$$tag" DOT_TAG=`echo $$tag | sed -e 's/-/\./'`

do_release:
	cvs tag $(MODNAME)-$(FREEZE_TAG)
	@tar -cf $(MODNAME)-$(DOT_TAG).tar $(RELEASE_FILES) 
	@mkdir $(MODNAME)-$(DOT_TAG)
	@mv $(MODNAME)-$(DOT_TAG).tar $(MODNAME)-$(DOT_TAG)
	@(cd $(MODNAME)-$(DOT_TAG); \
	 tar -xf $(MODNAME)-$(DOT_TAG).tar; rm -f $(MODNAME)-$(DOT_TAG).tar)
	@tar -cf $(MODNAME)-$(DOT_TAG).tar $(MODNAME)-$(DOT_TAG)
	@gzip $(MODNAME)-$(DOT_TAG).tar
	@mv $(MODNAME)-$(DOT_TAG).tar.gz $(MODNAME)-$(DOT_TAG).tgz
	@uuencode $(MODNAME)-$(DOT_TAG).tgz $(MODNAME)-$(DOT_TAG).tgz > $(MODNAME)-$(DOT_TAG).tgz.uu
	@rm -fr $(MODNAME)-$(DOT_TAG)
	@echo "*** Created file: $(MODNAME)-$(DOT_TAG).tgz"
	@echo "***     and file: $(MODNAME)-$(DOT_TAG).tgz.uu"

