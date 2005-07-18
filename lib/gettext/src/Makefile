
include ../../../support/include.mk

include ../vsn.mk
VSN=$(GETTEXT_VSN)

MODULES=gettext \
	gettext_compile \
	gettext_server \
	gettext_sup \
	gettext_app \
	iso639

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/gettext.app

ERLC_FLAGS+=-W

INCLUDES=

#
# Targets
#

all: $(EBIN_FILES)

gettext_compile: ../ebin/gettext_compile.$(EMULATOR)

debug:
	$(MAKE) ERLC_FLAGS+="$(ERLC_FLAGS) +debug_info -Ddebug"

imsg:
	$(MAKE) ERLC_FLAGS+="-Dimsg"

clean:
	rm -f $(EBIN_FILES)

../ebin/*.$(EMULATOR): $(INCLUDES)
