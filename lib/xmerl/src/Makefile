include ../../../support/include.mk

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
include ../vsn.mk
VSN=$(XMERL_VSN)

RELSYSDIR = $(RELEASE_PATH)/lib/xmerl-$(VSN)

EBIN = ../ebin

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	test \
	test_html \
	xmerl \
	xmerl_lib \
	xmerl_eventp \
	xmerl_scan \
	xmerl_xlate \
	xmerl_xml \
	xmerl_html \
	xmerl_text \
	xmerl_xpath \
	xmerl_xpath_parse \
	xmerl_xpath_pred \
	xmerl_xpath_scan


HRL_FILES = xmerl.hrl xmerl_xlink.hrl

INTERNAL_HRL_FILES  = 

ERL_FILES= $(MODULES:%=%.erl)

APP_FILE= xmerl.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= 
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(DEBUG) -I ../inc


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
all:	$(TARGET_FILES)

opt build: $(TARGET_FILES) 

clean:
	$(RM) $(TARGET_FILES)
	$(RM) xmerl_xpath_parse.erl

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

xmerl_xpath_parse.erl:	xmerl_xpath_parse.yrl
	$(ERLC) $(ERL_FLAGS) $(ESRC) $<


$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
#ifeq "${MA_TOP}" ""
#MA_TOP = ../../..
#endif
#include $(MA_TOP)/config/make.release


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin


#------------------------------------------------------------
# .hrl dependencies


