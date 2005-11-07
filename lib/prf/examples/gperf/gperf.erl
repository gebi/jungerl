-module(gperf).
-export([start/1]).

start([Node]) -> prfHost:start([gperf,Node,gperfConsumer]);
start(Node) -> prfHost:start(gperf,Node,gperfConsumer).
