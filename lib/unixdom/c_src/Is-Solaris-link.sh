#!/bin/sh

case `uname -s` in
    SunOS)
        echo "-lnsl -lsocket -lpthread"
        ;;
    FreeBSD)
	echo "-lc_r"
	;;
    *)
        echo "-lpthread"
        ;;
esac
