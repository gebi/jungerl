#!/bin/sh
basedir=$(dirname $0)
erl -pz ${basedir}/../ebin -pz ${basedir}/../../rpc/ebin \
    -s nfs_procfs start_link "$@"

