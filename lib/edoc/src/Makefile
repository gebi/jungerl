include ../../../support/include.mk

INCLUDE = ../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES := $(ERL_SOURCES)
OBJECTS := $(ERL_OBJECTS) $(EBIN_DIR)/edoc_parser.$(EMULATOR)

all: $(OBJECTS) 

edoc_parser.erl: edoc_parser.yrl
	$(ERLC) $<
edoc.hrl: ../include/edoc_doclet.hrl
$(EBIN_DIR)/edoc.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_data.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_doclet.$(EMULATOR): ../include/edoc_doclet.hrl ../include/xmerl.hrl
$(EBIN_DIR)/edoc_extract.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_layout.$(EMULATOR): ../include/xmerl.hrl
$(EBIN_DIR)/edoc_lib.$(EMULATOR): edoc.hrl ../include/xmerl.hrl
$(EBIN_DIR)/edoc_macros.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_parse_xml.$(EMULATOR): ../include/xmerl.hrl
$(EBIN_DIR)/edoc_parser.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_refs.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_report.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_tags.$(EMULATOR): edoc.hrl
$(EBIN_DIR)/edoc_types.$(EMULATOR): edoc.hrl ../include/xmerl.hrl
$(EBIN_DIR)/edoc_wiki.$(EMULATOR): edoc.hrl

clean:
	rm -f $(OBJECTS) edoc_parser.erl

$(EBIN_DIR)/%.$(EMULATOR):%.erl
	erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN_DIR) $<
