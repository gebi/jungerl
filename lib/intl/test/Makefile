
MO_FILES = sv/LC_MESSAGES/test.mo

all: $(MO_FILES) test.beam

# Swedish
sv/LC_MESSAGES/test.mo:	sv/LC_MESSAGES/test.po 
	msgfmt -o$@ $<

test.beam:	test.erl
	erlc -o$@ $<


