# Update the XMERL_PATH to point at your xmerl installation.
XMERL_PATH=../../xmerl

# Do not change anything below this line.
ifeq ($(DEBUG),true)
DEBUG_FLAGS = -DDEBUG
else
DEBUG_FLAGS =
endif

ERLC=erlc
ERLC_FLAGS=-W $(DEBUG_FLAGS) -I $(XMERL_PATH)/inc -o ../ebin
MODULES=xmlrpc xmlrpc_decode xmlrpc_encode xmlrpc_http xmlrpc_util tcp_serv
EBIN_FILES=$(MODULES:%=../ebin/%.beam)

all: $(EBIN_FILES) 

../ebin/%.beam: %.erl log.hrl
	$(ERLC) $(ERLC_FLAGS) $<

clean:
	rm -f $(EBIN_FILES)
