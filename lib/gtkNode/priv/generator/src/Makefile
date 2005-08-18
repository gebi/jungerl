TOP = $(shell dirname `pwd`)
TOP_TARG = $(shell dirname $(TOP))
GEN = $(TOP)/gen
GEN_TARG = $(TOP_TARG)/gen
STRUCTS = $(TOP)/../../src/gtkNode_structs.c
GTKINCS = $(shell pkg-config --cflags libglade-2.0)
GTKINC = $(shell ./get_inc_path gtk+-2.0 gtk-2.0)
GINC = $(shell ./get_inc_path glib-2.0 glib-2.0)
GTK_HS = $(wildcard $(GTKINC)/gtk/*.h)
GDK_PIXBUF_HS = $(wildcard $(GTKINC)/gdk-pixbuf/*.h)
GDK_HS = $(wildcard $(GTKINC)/gdk/*.h)
G_HS = $(wildcard $(GINC)/*/*.h)
GTK_WHITE_HS = $(patsubst %, $(GTKINC)/%, $(shell cat gtk_white.txt))
GDK_WHITE_HS = $(patsubst %, $(GTKINC)/%, $(shell cat gdk_white.txt))
G_WHITE_HS = $(patsubst %, $(GINC)/%, $(shell cat g_white.txt))
G_DEFS = $(GEN)/g.defs $(GEN)/g_white.defs 
GDK_DEFS = $(GEN)/gdk.defs $(GEN)/gdk_white.defs
GTK_DEFS = $(GEN)/gtk.defs $(GEN)/gtk_white.defs
GEN_FILES = gtk_generated gdk_generated g_generated
GEN_HS = $(patsubst %, $(GEN)/%.h, $(GEN_FILES))
TARG_HS = $(patsubst %, $(GEN_TARG)/%.h, $(GEN_FILES))

ERLC = erlc
ERLC_FLAGS = -W +debug_info
EBIN = $(TOP)/ebin
ERLS  = $(wildcard *.erl)
EFILES =$(patsubst %.erl, %, $(ERLS))
ECOMPILE = $(ERLC) -o $(EBIN) $(ERLC_FLAGS)

.PHONY: default generate install

default: generate
generate:  $(GTK_DEFS) $(GDK_DEFS) $(G_DEFS) $(EFILES) $(GEN_HS)
install:  $(TARG_HS)

clean:
	\rm $(EBIN)/*.beam
	\rm $(GEN)/*

$(EFILES): % : $(EBIN)/%.beam 

$(EBIN)/%.beam: %.erl
	$(ERLC) -o $(EBIN) $<

$(GEN)/gtk.defs: $(GTK_HS) 
	cp gtk.defs $@
	./h2def.py $(GTK_HS) >> $@
$(GEN)/gdk.defs: $(GDK_HS) $(GDK_PIXBUF_HS) gdk.defs
	cp gdk.defs $@
	./h2def.py $(GDK_HS) $(GDK_PIXBUF_HS) >> $@
$(GEN)/g.defs: $(G_HS)
	cp g.defs $@
	./h2def.py $(G_HS) >> $@

$(GEN)/gtk_white.defs: $(GTK_WHITE_HS) gtk_white.txt
	./h2def.py $(GTK_WHITE_HS) > $@
$(GEN)/gdk_white.defs: $(GDK_WHITE_HS) gdk_white.txt
	./h2def.py $(GDK_WHITE_HS) > $@
$(GEN)/g_white.defs: $(G_WHITE_HS) g_white.txt
	./h2def.py $(G_WHITE_HS) > $@

$(GEN_HS): $(EBIN)/generator.beam $(GTK_DEFS) $(GDK_DEFS) $(G_DEFS) $(STRUCTS)
	erl -pa $(EBIN) -s generator go -s erlang halt

$(TARG_HS) : $(GEN_TARG)/%: $(GEN)/%
	cp $< $@
