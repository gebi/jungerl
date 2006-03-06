
SUBDIRS = src
APPNAME=plain_fsm
BINDIR=ebin
INCDIR=inc
DOCDIR=doc
EDOCDIR="../edoc/ebin"
STOOLSDIR="../syntax_tools/ebin"
XMERLDIR="../xmerl/ebin"
DOC_OPTS=[{title,"Welcome to Plain_FSM"}]

all: all-subdirs

# Commented out since the new edoc is not yet in jungerl. See the README
#docs:
	erl -noshell -pa $(EDOCDIR) -pz $(STOOLSDIR) -pz $(XMERLDIR) -run edoc_run application "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s erlang halt

all-subdirs:
	@for dir in $(SUBDIRS); do \
		(cd $$dir; $(MAKE) $(MAKETARGET) EBIN=../$(BINDIR) INCLUDE=../$(INCDIR)); \
	done

clean:
	$(MAKE) MAKETARGET="distclean" all-subdirs

distclean:
	$(MAKE) MAKETARGET="distclean" all-subdirs

realclean:
	$(MAKE) MAKETARGET="realclean" all-subdirs
	-rm -f $(DOCDIR)/*.html $(DOCDIR)/edoc-info $(DOCDIR)/stylesheet.css
