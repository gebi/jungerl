-module(pico_test).

%% Copyright (C) 1999, Bluetail AB
%% File    : pico_test.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%% Purpose : Test program for small web server

-export([batch/1, start/0, stop/0]).
-export([start_handler/1, event_handler/2, stop_handler/2]).

-import(pico_utils, [body/1, classify/1, header/1, show/1]).

batch(_) -> start().
start()  -> pico_http_server:start(4999, 20, ?MODULE, [1,2,3]).
stop()   -> pico_http_server:stop(4999, foo1234).
    
start_handler(A) -> 1.

event_handler({get,_,File,Args}, State) ->
    {get_file(File, Args), State+1};
event_handler(Event, State) ->
    io:format("callback event:~p State:~p~n",[Event, State]),
    {show({pico_test,event, Event,state,State}), State+1}.

stop_handler(Reason, State) ->
    io:format("Stopping:~p ~p~n", [Reason, State]),
    {stopoyyes, State}.

get_file(F,Args) ->
    case file:read_file("." ++ F) of
	{ok, Bin} ->
	    case classify(F) of
		html ->
		    [header(html),Bin];
		jpg ->
		    [header(jpg),Bin];
		gif ->
		    [header(jpg),Bin];
		_ ->
		    [header(text),body("white"),"<pre>",Bin,"</pre>"]
	    end;
	_ ->
	    show({no_such_file,F,args,Args,cwd,file:get_cwd()})
    end.

