-module(edit_make).
-export([start/0]).

start() ->
    spawn(fun() -> doit() end).

doit() ->
    sleep(1),
    ig:gen(edit_ig),
    c:c(edit_ig),
    halt().

sleep(Sec) ->
    receive after Sec*1000 -> true end.
