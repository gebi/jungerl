
MODULES = \
	bic_leex \
	bic \
	bic_cpp \
	bic_scan \
	bic_parse

ERL_FLAGS = \
	-I ../include -W0

LEEX = -pa ../ebin

override ERLC_FLAGS += $(ERL_FLAGS)

debug: ERLC_FLAGS += +debug_info -Ddebug -W

OBJS = $(MODULES:%=../ebin/%.beam)

all: $(OBJS)

debug: all

depend:
	edep -MM -o ../ebin $(ERLC_FLAGS) $(MODULES:%=%.erl) > depend.mk

dialyze:
	dialyzer --src -o dia.out $(ERLC_FLAGS) -c $(MODULES:%=%.erl) 

clean:
	rm -f $(OBJS) bic_scan.erl bic_parse.erl


-include depend.mk


../ebin/bic_scan.beam:	bic_scan.erl
	erlc -o ../ebin $(LEEX) $(ERLC_FLAGS) $<

bic_scan.erl:	bic_scan.xrl
	-erl -noshell $(LEEX) $(ERL_FLAGS) -run bic_leex gen $< $@ -run init stop

../ebin/bic_parse.beam: bic_parse.erl

bic_parse.erl: bic_parse.yrl
	erlc $<

../ebin/%.beam:	%.erl
	erlc -o ../ebin $(ERLC_FLAGS) $<

