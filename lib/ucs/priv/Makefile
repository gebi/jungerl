ifeq "${MA_TOP}" ""
MA_TOP = ../../..
ERLC = erlc
ERL = erl
ETAGS = etags
EMULATOR = beam
EBIN = ../ebin
INSTALL_DIR = install -c -d
INSTALL_DATA = install -c -m 644
MA_TOP = ../../..
endif

-include $(MA_TOP)/config/make.variables


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
include ../vsn.mk

RELSYSDIR = $(RELEASE_PATH)/lib/ucs-$(UCS_VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

UCS_MNEM_TARGETS = ucsmnem1 ucsmnem2
DBS_TARGETS = unidata $(UCS_MNEM_TARGETS) mibenum


TARGET_FILES = $(DBS_TARGETS:%=db/%.dets)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -noshell -pa ../ebin
ERL_COMPILE_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all: opt


opt build: $(TARGET_FILES)


clean:
	$(RM)  $(TARGET_FILES) db/*.tab

docs:

test:  $(TARGET_FILES) unicodemappings

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
db/unidata.dets: UnicodeData.txt
	$(ERL) $(ERL_FLAGS) -s ucs rebuild_database unidata -s init stop

db/ucsmnem1.dets: db/unidata.dets ucsmnem.edat
	$(ERL) $(ERL_FLAGS) -s ucs rebuild_database mnemonics -s init stop

db/ucsmnem2.dets: db/unidata.dets ucsmnem.edat
	$(ERL) $(ERL_FLAGS) -s ucs rebuild_database mnemonics -s init stop

db/mibenum.dets: character-sets character-sets-not-IANA
	$(ERL) $(ERL_FLAGS) -s ucs rebuild_database mibenum -s init stop

unicodemappings: 
	$(ERL) $(ERL_FLAGS) -s ucs rebuild_database mappings testdata -s init stop


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
ifeq "${MA_TOP}" ""
MA_TOP = ../../..
endif

-include $(MA_TOP)/config/make.release

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/priv
