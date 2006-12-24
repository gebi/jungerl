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
    
start_handler(_) -> 1.

% handler for GET method; return contents of named file
event_handler({get,_,File,Args}, State) ->
    {get_file(File, Args), State+1};
% dummy handler for POST method; just prints action URL and parameters
event_handler({post,_,File,Args}, State) ->
    {[header({ok,text}),show({post_action,File,args,Args})], State+1};
event_handler(Event, State) ->
    io:format("callback event:~p State:~p~n",[Event, State]),
    {show({pico_test,event,Event,state,State}), State+1}.

stop_handler(Reason, State) ->
    io:format("Stopping:~p ~p~n", [Reason, State]),
    {stopoyyes, State}.

get_file(F,_Args) ->
    Path = "htdocs" ++ F,
    case file:read_file(Path) of
	{ok, Bin} ->
	    io:format("found ~s\n", [Path]),
	    [header({ok,classify(F)}),Bin];
	_ ->
	    io:format("did not find ~s\n", [Path]),
	    [header({error,"404 Not Found",
                show({no_such_file,F,cwd,file:get_cwd()})})]
    end.

