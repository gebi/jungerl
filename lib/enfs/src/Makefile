include ../../../support/include.mk

# Some modules are automatically generated, so we won't use the
# include makefile's auto-detected list.
MODULES := nfs_svc nfs_xdr nfs_server nfs_procfs 
OBJECTS := $(MODULES:%=../ebin/%.$(EMULATOR))

all: $(OBJECTS)

# Do this as root
mount:
	mount -o noac,port=22049,mountport=22050,soft,udp,mountvers=1,nfsvers=2 localhost:/procfs mnt

clean:
	rm -f $(OBJECTS) nfs_svc.erl nfs_xdr.erl nfs.hrl

nfs.hrl:		nfs.x
	../../rpc/priv/erpcgen -a '[hrl]' nfs

nfs_svc.erl:	nfs.x
	../../rpc/priv/erpcgen -a '[svc]' nfs

nfs_xdr.erl:	nfs.x
	../../rpc/priv/erpcgen -a '[xdr]' nfs

../ebin/nfs_server.$(EMULATOR): nfs.hrl
../ebin/nfs_svc.$(EMULATOR): nfs.hrl

