# FIXME These variables should be automatically set when compiling
ERL = erl
ERLC = erlc
ETAGS = etags
EMULATOR = beam
EBIN = ../ebin
ESRC = ./
DOCDIR = ../doc
RM = rm -f
INSTALL = /usr/bin/install -c
INSTALL_DIR = /usr/bin/install -c -d
INSTALL_DATA = ${INSTALL} -m 644


### XMERL use EDOC for documentation, to regenerate update paths as needed!
XMERL_APP = ..
# XMERL_APP = ../../../../../ma/src/external/xmerl
EDOC_APP = ../../edoc
UCS_APP = ../../ucs
SYNTAX_TOOLS_APP = ../../../../../ma/src/external/syntax_tools



# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
include ../vsn.mk
VSN=$(XMERL_VSN)

RELSYSDIR = $(RELEASE_PATH)/lib/xmerl-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	xmerl_scan \
	xmerl \
	xmerl_xs \
	xmerl_eventp \
	xmerl_xpath \
	xmerl_lib \
	xmerl_xlate \
	xmerl_xml \
	xmerl_sgml \
	xmerl_html \
	xmerl_text \
	xmerl_otpsgml \
	xmerl_simple \
	xmerl_validate \
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

EDOC_PATHS = \
	-pa $(EDOC_APP)/ebin -pa $(XMERL_APP)/ebin \
	-pa $(SYNTAX_TOOLS_APP)/ebin \
	-pa $(UCS_APP)/ebin


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
# DEBUG = -Ddebug
ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(DEBUG) -I ../inc


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
all:	$(TARGET_FILES)

doc:	
	$(ERL) -noshell $(EDOC_PATHS) \
		-run edoc_run application "xmerl" "\"../../xmerl\"" \
		"[{preprocess,true},{includes,[\"../../xmerl/inc\"]}]" \
		-s erlang halt


opt build: $(TARGET_FILES) 

clean:
	$(RM) $(TARGET_FILES)
	$(RM) xmerl_xpath_parse.erl

realclean: clean
	$(RM) $(DOC_TARGET_FILES)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

xmerl_xpath_parse.erl:	xmerl_xpath_parse.yrl
	$(ERLC) $(ERL_FLAGS) -o  $(ESRC) $<


$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

# $(DOCDIR)/%.html:	%.erl
#	$(ERL) -noshell $(EDOC_PATHS) \
#		-run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}]' -s erlang halt


$(DOCDIR)/%.html:	%.erl
	$(ERL) $(EDOC_PATHS) -s edoc file $< ['{dir,"$(DOCDIR)"}'] -s erlang halt


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
#ifeq "${MA_TOP}" ""
#MA_TOP = ../../..
#endif
#include $(MA_TOP)/config/make.release

release_src:
	$(INSTALL_DATA) $(MODULES:%=%.erl) $(RELSYSDIR)/src
	rm -f $(RELSYSDIR)/src/xmerl_xpath_parse.erl
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/src
	$(INSTALL_DATA) $(APP_SRC) $(RELSYSDIR)/src
	$(INSTALL_DATA) xmerl_xpath_parse.yrl $(RELSYSDIR)/src


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin


#------------------------------------------------------------
# .hrl dependencies
$(EBIN)/xmerl.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_lib.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_scan.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_xlate.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_xml.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_html.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_text.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_eventp.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_simple.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_xpath.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_xpath_pred.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_xpath_scan.beam:../inc/xmerl.hrl
$(EBIN)/xmerl_validate.beam:../inc/xmerl.hrl


