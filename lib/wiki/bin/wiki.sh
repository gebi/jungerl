#!/bin/sh
 
##
## usage wiki.sh {start|stop|debug}
##
 
## These point to where
## The the wiki code and database is store
##   WIKI  = path to the wiki ebin
##   PICO  = path to the pico ebin
##   STORE = path to the wiki store
##   PORT  = listen port

# These paths are set up to work from a checked out jungerl source tree.
# Run this script with jungerl/lib/wiki/ as current directory.
WIKI=./ebin
PICO=../pico/ebin
STORE=$WIKI/../store
PORT=4999
ERL=/usr/local/bin/erl

export HEART_COMMAND="$WIKI/wiki.sh start"

case $1 in
  start)
    ${ERL} -boot start_sasl -sname wiki001 -pa ${WIKI} -pa ${PICO} \
           -s wiki start ${PORT} ${STORE} -heart -detached
    echo  "Starting Wiki"
    ;;
 
  debug)
    # if a second parameter is given, give it to -sname option
    ${ERL} ${2:+-sname $2} -pa ${WIKI} -pa ${PICO} -s wiki start ${PORT} ${STORE}
    ;;
 
  stop)
    ${ERL} -noshell -sname wiki_stopper -pa ${WIKI} -pa ${PICO} \
           -s wiki stop wiki001@`hostname`
    echo "Stopping wiki"
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0
