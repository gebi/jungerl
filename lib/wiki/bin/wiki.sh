#!/bin/sh

#!/bin/sh
 
##
## usage wiki.sh {start|stop|debug}
##
 
## These point to where
## The the wiki code and database is store
##   PA1   = path to the wiki program
##   PA2   = path to the pico server
##   STORE = path to the wiki store
##   PORT  = port to run as
 
WIKI=$HOME/erl/erl.now/wiki-12.0
PICO=$HOME/erl/erl.now/pico-11.0
STORE=$WIKI/store
PORT=4999
ERL=/usr/local/bin/erl

export HEART_COMMAND="$WIKI/wiki.sh start"

case $1 in
  start)
    ${ERL} -boot start_sasl -sname wiki001 -pa ${WIKI} -pa ${PICO} \
           -s wiki start ${PORT} ${STORE}  -heart -detached
    echo  "Starting Wiki"
    ;;
 
  debug)
    ${ERL} -sname $2 -pa ${PA1} -pa ${PA2} -s wiki start ${PORT} ${STORE}
    ;;
 
  stop)
    ${ERL} -noshell -sname wiki_stopper -pa ${WIKI} -pa {PICO} \
           -s wiki stop wiki001@pippi
    echo "Stopping wiki"
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0
                    
