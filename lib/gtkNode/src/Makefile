CC = gcc -g -c
LD = gcc -g 
BIN = ../priv/bin
EBIN = ../ebin
OBJ = ../priv/obj
INC = ../include
GEN = ../priv/gen

OTP_ROOT = $(shell ./check_otp -root)
EI_ROOT = $(shell ./check_otp -ei)
GTKINCS = `pkg-config --cflags libglade-2.0`
GTKLIBS = `pkg-config --libs gmodule-2.0 libglade-2.0` 
GTKINC = $(shell ./get_inc_path gtk+-2.0 gtk-2.0)
GLIBINC = $(shell ./get_inc_path glib-2.0 glib-2.0)

ERLC = $(OTP_ROOT)/bin/erlc
EFLAGS = +debug_info
EILIB = $(EI_ROOT)/lib
EIINC = $(EI_ROOT)/include

GENS = $(GEN)/gtk_generated.h $(GEN)/gdk_generated.h $(GEN)/g_generated.h
IFLAGS = -I$(INC) -I$(GEN) -I$(EIINC) $(GTKINCS)
LFLAGS = $(GTKLIBS) -L$(EILIB) -lei -lpthread 
CINCS  = $(wildcard $(INC)/*.h) 
CS     = $(wildcard *.c)
OBJS   = $(patsubst %.c, $(OBJ)/%.o, $(CS))

OUT = $(BIN)/gtkNode-$(shell uname|tr A-Z a-z)

.PHONY: check gtkNode default all clean

default: all
all:  check TAGS gtkNode $(EBIN)/gtkNode.beam

check:
	./check
clean:
	-rm -f $(BIN)/gtkNode*
	-rm -f $(EBIN)/*.beam
	-rm -f $(OBJ)/*.o

TAGS : $(CS)
	etags $(CS)

gtkNode : $(OUT)

$(OBJ)/gtkNode_gen.o : gtkNode_gen.c $(CINCS) $(GENS)
	$(CC) -o $@ $< $(IFLAGS)
$(OBJ)/%.o : %.c $(CINCS)
	$(CC) -o $@ $< $(IFLAGS)
$(OUT) : $(OBJS)
	$(LD) -o $@ $(OBJS) $(LFLAGS) 
$(EBIN)/gtkNode.beam : gtkNode.erl
	$(ERLC) -o $(EBIN) $(EFLAGS) $<
