include ../../../support/include.mk

all: xmlrpc.txt xmlrpc.ps xmlrpc.pdf

xmlrpc.txt: xmlrpc.3
	$(GROFF) -t -e -mandoc -Tascii xmlrpc.3 | col -bx > xmlrpc.txt

xmlrpc.ps: xmlrpc.3
	$(GROFF) -t -e -mandoc -Tps xmlrpc.3 > xmlrpc.ps

xmlrpc.pdf: xmlrpc.ps
	$(PS2PDF) xmlrpc.ps xmlrpc.pdf

clean:
	rm -f xmlrpc.txt xmlrpc.ps xmlrpc.pdf
