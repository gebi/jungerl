
MODULES = \
	intl \
	eintl \
	mo \
	egettext


ERLC_FLAGS = -Ddebug

OBJS = $(MODULES:%=../ebin/%.beam)

all: $(OBJS)

clean:
	rm -f $(OBJS)

../ebin/%.beam:	%.erl
	erlc -o ../ebin $(ERLC_FLAGS) $<
