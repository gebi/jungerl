include ../../../support/include.mk

ERLC_FLAGS += -I ../.. -pa ../ebin/

SCRIPT=../bin/ermacs
SED_EXPR = s:%BASEDIR%:`pwd`/..:

all: $(ERL_OBJECTS) $(SCRIPT)

# edit_transform has to be built first. This rule is actually
# circular, but that seems okay with GNU make.
$(ERL_OBJECTS): ../ebin/edit_transform.beam

$(SCRIPT): ermacs.in
	sed ${SED_EXPR} < $< > $@
	chmod +x $@

clean:
	-rm -f $(ERL_OBJECTS)
	-rm -f ../bin/ermacs

