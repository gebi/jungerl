include ../../../support/include.mk

XMERL_PATH=../../xmerl

all: $(ERL_OBJECTS)

clean:
	rm -f $(ERL_OBJECTS)

ERLC_FLAGS += -I $(XMERL_PATH)/inc

# Add dependency on the automatically-found headers
$(ERL_OBJECTS): $(ERL_HEADERS)

