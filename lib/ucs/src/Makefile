
ifeq "${MA_TOP}" ""
MA_TOP = ../../..
ERLC = erlc
ETAGS = etags
EMULATOR = beam
EBIN = ../ebin
INSTALL_DIR = install -c -d
INSTALL_DATA = install -c -m 644
else
-include $(MA_TOP)/config/make.variables
endif


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
include ../vsn.mk

RELSYSDIR = $(RELEASE_PATH)/lib/ucs-$(UCS_VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	ucs \
	ucs_app \
	ucs_sup \
	ucs_data \
	ucs_data_build


HRL_FILES = unidata.hrl

INTERNAL_HRL_FILES  = ucs_data.hrl

ERL_FILES= $(MODULES:%=%.erl)

APP_FILE= ucs.app
APP_SRC= $(APP_FILE).src
APP_TARGET= ../ebin/$(APP_FILE)

APPUP_FILE= 
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= ../ebin/$(APPUP_FILE)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -W
ERL_COMPILE_FLAGS += -I ../include
# ERL_COMPILE_FLAGS += 


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
all:	$(TARGET_FILES)

opt build: $(TARGET_FILES) 

clean:
	$(RM) $(TARGET_FILES) 


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(UCS_VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(UCS_VSN);' $< > $@


$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
-include $(MA_TOP)/config/make.release

release_src:
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/src
	$(INSTALL_DATA) $(APP_SRC) $(RELSYSDIR)/src


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

