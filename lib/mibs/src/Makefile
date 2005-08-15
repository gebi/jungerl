MODULES = \
	event_mib \
	event_mib_cfg

ERL_FILES = $(MODULES:%=%.erl)

HRLS =  event_mib.hrl

HRL_FILES = $(HRLS:%=%.hrl)

TARGET_FILES = $(MODULES:%=../ebin/%.beam)

all: $(TARGET_FILES)

../ebin/%.beam: %.erl $(HRLS)
	erlc $(ERLC_FLAGS) -o ../ebin $<

clean:
	rm -f $(TARGET_FILES)
