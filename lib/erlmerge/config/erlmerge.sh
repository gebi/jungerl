#!/bin/sh

usage () {

cat <<EOF

erlmerge <options> <applications>

 where  <options>:

    -u             : Update application(s) to latest version
    -p             : Dry run only (pretend)
    -i,install     : Install the following application(s)
    -d,delete      : Delete the following application(s)
    -s,search      : Search for application(s)
    suicide        : Remove erlmerge and all non-original apps
    -y,sync        : Update the application database (syncronize)
    -x,dump <app>  : Dump info about application (debug)
    -w <Url>       : Url pointing to an erlmerge sync-file
    -z <Url>       : Proxy (host.domain or host.domain:port)

EOF
}

if [  -z $1 ] ; then
    usage
    exit 2
fi

EM_DRYRUN=false
EM_UPDATE=false
EM_URL="http://www.trapexit.org/trapexit.erlmerge"
EM_PROXY=
EM_CMD=
EM_ARGS=
EM_SUICIDE="n"

while [ "$1"x != x ]; do
   case $1 in
      -p)
	EM_DRYRUN=true
	;;
      -w)
	EM_URL=$2
	shift 1
	;;
      -z)
	EM_PROXY=$2
	shift 1
	;;
      -u)
	EM_UPDATE=true
	EM_CMD="install"
	;;
      -i | install)
	EM_CMD="install"
	;;
      -d | delete)
	EM_CMD="delete"
	;;
      -x | dump)
	EM_CMD="dump"
	;;
      -y | sync)
	EM_CMD="sync"
	;;
      -s | search)
	EM_CMD="search"
	;;
      suicide)
	EM_CMD="suicide"
	EM_SUICIDE="y"
	;;
      -h)
	usage
	exit
	;;
      setup)             # Used when installing erlmerge
	EM_CMD="setup"
	;;
      *)
	EM_ARGS="${EM_ARGS} $1"
	;;
   esac
   shift 1
done

export EM_DRYRUN
export EM_UPDATE
export EM_URL
export EM_PROXY
export EM_CMD
export EM_ARGS
