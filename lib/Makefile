LIBS := tuntap lersp slang ermacs

all: $(LIBS)

clean conf:
	@for dir in $(LIBS); do \
	  if [ ! -e $@/SKIP ]; then \
	    (cd $$dir; $(MAKE) $@) \
	  fi \
	done

$(LIBS):
	@if [ ! -e $@/SKIP ]; then \
	  (cd $@; $(MAKE) all) \
	fi

# Dependencies between programs.

ermacs: slang

.PHONY: $(LIBS)

