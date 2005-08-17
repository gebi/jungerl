ERLC = erlc
CC = gcc -g -c
LD = gcc -g 
BIN = ../priv/bin
EBIN = ../ebin
OBJ = ../priv/obj
INC = ../include
GEN = ../priv/gen

GTKINCS = `pkg-config --cflags libglade-2.0`
GTKLIBS = `pkg-config --libs gmodule-2.0 libglade-2.0` 
GTKINC = $(shell ./get_inc_path gtk+-2.0 gtk-2.0)
GLIBINC = $(shell ./get_inc_path glib-2.0 glib-2.0)

EILIB = $(wildcard $(OTP_ROOT)/lib/erlang/lib/erl_interface-*/lib)
EIINC = $(wildcard $(OTP_ROOT)/lib/erlang/lib/erl_interface-*/include)

IFLAGS = -I$(INC) -I$(GEN) -I$(EIINC) $(GTKINCS)
LFLAGS = $(GTKLIBS) -L$(EILIB) -lei #-lsocket 
CINCS  = $(wildcard $(INC)/*.h)
CINCS += $(GEN)/gtk_generated.h $(GEN)/gdk_generated.h $(GEN)/g_generated.h
CS     = $(wildcard *.c)
OBJS   = $(patsubst %.c, $(OBJ)/%.o, $(CS))

.PHONY: gtkNode default all 

default: all
all:  TAGS gtkNode $(EBIN)/gtkNode.beam

clean:
	\rm $(BIN)/*
	\rm $(OBJ)/*.o

TAGS : $(CS)
	etags $(CS)
gtkNode : $(BIN)/gtkNode

$(OBJ)/%.o : %.c $(CINCS)
	$(CC) -o $@ $< $(IFLAGS)
$(BIN)/gtkNode : $(OBJS)
	$(LD) -o $@ $(OBJS) $(LFLAGS) 
$(EBIN)/gtkNode.beam : gtkNode.erl
	$(ERLC) -o $(EBIN) $<
