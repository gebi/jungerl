
YAWS_INC=/usr/local/lib/yaws/include

##
## Create a timestamp file as a result of checking
## the yaws-file, so that we don't have to re-compile
## everything, all the time.
##
.SUFFIXES: .yaws-TS .yaws

all: $(shell echo *.yaws | sed 's/yaws/yaws-TS/g')

clean:  
	-rm *.yaws-TS

%.yaws-TS: %.yaws
	yaws -check $< $(YAWS_INC) ../../include || exit 1; \
	touch $<-TS


