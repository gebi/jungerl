# erlang
ERLC = erlc
ERLC_FLAGS = -W +debug_info
EBIN = $(TOP)/ebin
ERLS  = $(wildcard *.erl)
EFILES =$(patsubst %.erl, %, $(ERLS))
ECOMPILE = $(ERLC) -o $(EBIN) $(ERLC_FLAGS)

# directory definitions
TOP = $(shell dirname `pwd`)
TOP_TARG = $(shell dirname $(TOP))
GEN = $(TOP)/gen
GEN_TARG = $(TOP_TARG)/gen
GTKINCS = $(shell pkg-config --cflags libglade-2.0)
GTKINC = $(shell ./get_inc_path gtk+-2.0 gtk-2.0)
GINC = $(shell ./get_inc_path glib-2.0 glib-2.0)

# input files
STRUCTS = $(TOP)/../../src/gtkNode_structs.c
GTK_HS = $(wildcard $(GTKINC)/gtk/*.h)
GDK_PIXBUF_HS = $(wildcard $(GTKINC)/gdk-pixbuf/*.h)
GDK_HS = $(wildcard $(GTKINC)/gdk/*.h)
G_HS = $(wildcard $(GINC)/*/*.h)
GTK_WHITE_HS = $(patsubst %, $(GTKINC)/%, $(shell cat gtk_white.txt))
GDK_WHITE_HS = $(patsubst %, $(GTKINC)/%, $(shell cat gdk_white.txt))
G_WHITE_HS = $(patsubst %, $(GINC)/%, $(shell cat g_white.txt))

# intermediate output files
VSN = $(shell pkg-config --modversion gtk+-2.0)
G_I = $(GEN)/g-$(VSN).defs
GDK_I = $(GEN)/gdk-$(VSN).defs 
GTK_I = $(GEN)/gtk-$(VSN).defs 
G_W_I = $(GEN)/g_white-$(VSN).defs 
GDK_W_I = $(GEN)/gdk_white-$(VSN).defs
GTK_W_I = $(GEN)/gtk_white-$(VSN).defs
INTERMEDIATES = $(G_I) $(GDK_I) $(GTK_I) $(G_W_I) $(GDK_W_I) $(GTK_W_I)

# final output files
GEN_FILES = gtk_generated gdk_generated g_generated
GEN_HS = $(patsubst %, $(GEN)/%-$(VSN).h, $(GEN_FILES))
TARG_HS = $(patsubst %, $(GEN_TARG)/%-$(VSN).h, $(GEN_FILES))

.PHONY: default generate install clean

default: generate
generate: $(INTERMEDIATES)  $(EFILES) $(GEN_HS)
install:  $(TARG_HS)

clean:
	-rm $(EBIN)/*.beam
	-rm $(GEN)/*

$(EFILES): % : $(EBIN)/%.beam 

$(EBIN)/%.beam: %.erl
	$(ERLC) -o $(EBIN) $<

$(GTK_I): $(GTK_HS) gtk.defs
	cp gtk.defs $@
	./h2def.py $(GTK_HS) >> $@
$(GDK_I): $(GDK_HS) $(GDK_PIXBUF_HS) gdk.defs
	cp gdk.defs $@
	./h2def.py $(GDK_HS) $(GDK_PIXBUF_HS) >> $@
$(G_I): $(G_HS) g.defs
	cp g.defs $@
	./h2def.py $(G_HS) >> $@

$(GTK_W_I): $(GTK_WHITE_HS) gtk_white.txt
	./h2def.py $(GTK_WHITE_HS) > $@
$(GDK_W_I): $(GDK_WHITE_HS) gdk_white.txt
	./h2def.py $(GDK_WHITE_HS) > $@
$(G_W_I): $(G_WHITE_HS) g_white.txt
	./h2def.py $(G_WHITE_HS) > $@

$(GEN_HS): $(EBIN)/generator.beam $(INTERMEDIATES) $(STRUCTS)
	erl -pa $(EBIN) -s generator go -s erlang halt

$(TARG_HS) : $(GEN_TARG)/%: $(GEN)/%
	cp $< $@
