# Update the APACHE_XMLRPC_PATH variable to point at your Apache
# XML-RPC installation.
APACHE_XMLRPC_PATH=xmlrpc-1.1.jar

# Do not change anything below this line.
ERL=erl
ERLC=erlc
ERLC_FLAGS=-W
MODULES=test
EBIN_FILES=$(MODULES:%=%.beam)
JAVAC=javac
JAVAC_FLAGS=-classpath $(APACHE_XMLRPC_PATH):.
JAVA_FILES=T50 T60
CLASS_FILES=$(JAVA_FILES:%=%.class)

test: build
	$(ERL) -pa ../ebin -pa ../../xmerl/ebin -pa . -s test all -s erlang halt
build: $(EBIN_FILES) $(CLASS_FILES)

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<

%.class: %.java
	$(JAVAC) $(JAVAC_FLAGS) $<

clean:
	rm -f $(EBIN_FILES) $(CLASS_FILES)
