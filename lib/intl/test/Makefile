
MO_FILES = \
	locale/sv/LC_MESSAGES/test.mo \
	locale/en/LC_MESSAGES/test.mo \
	locale/fr/LC_MESSAGES/test.mo \
	locale/ja/LC_MESSAGES/test.mo 

all: $(MO_FILES) test.beam test

test.beam:	test.erl
	erlc -o$@ $<

test:	test.o
	$(CC) -o$@ test.o 

test.c.pot:
	xgettext -k_:1 -kN_:1 -o $@ test.c

test.pot:
	egettext -o $@ test.erl

# Compile messages
locale/%/LC_MESSAGES/test.mo:	po/%.po 
	mkdir -p `dirname $@`
	msgfmt -o$@ $<




