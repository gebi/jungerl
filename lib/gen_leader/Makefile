
SUBDIRS = src
APPNAME=gen_leader
BINDIR=ebin
INCDIR=inc
DOCDIR=doc
EDOCDIR="../edoc/ebin"
STOOLSDIR="../syntax_tools/ebin"
XMERLDIR="../xmerl/ebin"
DOCDIR=doc
DOC_OPTS=[{title,"Welcome to gen_leader"},{hidden,false},{private,false}]

all: all-subdirs docs


docs:
	erl -noshell -run edoc_run application "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s erlang halt

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
