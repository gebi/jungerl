
SUBDIRS = src
BINDIR = ebin

VSN = 0.6a (Jungerl)

APPNAME=edoc
DOC_OPTS=[{def,{vsn,"$(VSN)"}}]

include ../../support/subdir.mk

docs:
	erl -noshell -pa $(BINDIR) -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop

