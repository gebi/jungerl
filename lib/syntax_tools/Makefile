
SUBDIRS = src

VSN = 1.4a (Jungerl)

APPNAME = syntax_tools
DOC_OPTS = [{def,{vsn,"$(VSN)"}}]

include ../../support/subdir.mk

docs:
	erl -noshell -pa "$(BINDIR)" -run edoc_run application "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
