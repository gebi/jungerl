
EMULATOR = beam
EBIN = ../ebin

INCLUDE = ../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= edoc.erl \
	edoc_scanner.erl \
	edoc_parser.erl \
	edoc_layout.erl \
	edoc_lib.erl \
	edoc_run.erl edoc_report.erl edoc_extract.erl edoc_tags.erl \
	edoc_macros.erl edoc_doclet.erl edoc_data.erl edoc_refs.erl \
	edoc_types.erl edoc_parse_expr.erl edoc_parse_ref.erl \
	edoc_parse_spec.erl edoc_parse_typedef.erl edoc_parse_xml.erl \
	edoc_parse_contact.erl edoc_wiki.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

all: $(OBJECTS)

edoc.hrl: ../include/edoc_doclet.hrl
$(EBIN)/edoc.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_data.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_doclet.$(EMULATOR): ../include/edoc_doclet.hrl ../include/xmerl.hrl
$(EBIN)/edoc_extract.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_layout.$(EMULATOR): ../include/xmerl.hrl
$(EBIN)/edoc_lib.$(EMULATOR): edoc.hrl ../include/xmerl.hrl
$(EBIN)/edoc_macros.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_parse_xml.$(EMULATOR): ../include/xmerl.hrl
$(EBIN)/edoc_parser.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_refs.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_report.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_tags.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_types.$(EMULATOR): edoc.hrl ../include/xmerl.hrl
$(EBIN)/edoc_wiki.$(EMULATOR): edoc.hrl

clean:
	rm -f $(OBJECTS) edoc_parser.erl

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
