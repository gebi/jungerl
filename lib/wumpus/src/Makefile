include ../../../support/include.mk

# this is not the proper way to do it, but it works

../doc/%.html: %.erl
	${ERL} -noshell \
          -pa ../../syntax_tools/ebin \
          -pa ../../edoc/ebin \
          -pa ../../xmerl/ebin \
          -run edoc file $< -run init stop
	mv *.html ../doc/

all: $(ERL_OBJECTS) ../doc/wumpus.html

clean:
	-rm $(ERL_OBJECTS)
	-rm ../doc/wumpus.html

