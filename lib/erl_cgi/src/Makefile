include ../../../support/include.mk

ERL_CGI_ERLC_FLAGS = $(ERLC_FLAGS) -I../include
EBIN=../ebin

OBJS = \
	$(EBIN)/erl_cgi.beam \
	$(EBIN)/erl_cgi_srv.beam \
	$(EBIN)/stdio.beam \
	$(EBIN)/esp.beam \
	$(EBIN)/esp_html.beam \
	$(EBIN)/html_esp.beam \
	$(EBIN)/html.beam

all:	$(OBJS)

clean:
	rm $(OBJS)

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_CGI_ERLC_FLAGS) -o$(EBIN) $<
