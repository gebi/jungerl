#!/bin/sh
if [ $# == 1 ]; then
    mount -o noac,port=22049,mountport=22050,soft,udp,mountvers=1,nfsvers=2 \
	localhost:/procfs $1
else
    echo "Usage: $0 <mountpoint>"
fi

