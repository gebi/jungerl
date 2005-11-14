
include vsn.mk

VSN=$(YFRONT_VSN)
APP_NAME=yfront

all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd priv/docroot;$(MAKE) clean)

release: clean appfile
	sh ../../support/create_release.sh

appfile:
	(cd src;$(MAKE) ../ebin/yfront.app)


docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
	       '"."' '[{def,{vsn,"$(VSN)"}}]' -s init stop

