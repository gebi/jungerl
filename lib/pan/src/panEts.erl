%%%-------------------------------------------------------------------
%%% File    : panEts.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created : 14 Feb 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(panEts).

-define(TIME, 5000).
-define(SERVER_PROC, ?MODULE).

-export([server/2]).

server(Act, Arg) ->
    server_verify(),
    Ref = make_ref(),
    ?SERVER_PROC ! {self(), Ref, {Act, Arg}},
    receive 
	{Ref, Repl} -> Repl
    after ?TIME -> {error, timeout}
    end.

server_verify() ->
    case whereis(?SERVER_PROC) of
	undefined ->
	    Own = self(),
	    spawn(fun() -> server_init(Own) end),
	    receive
		{?SERVER_PROC, ok} -> ok
	    after ?TIME -> {error, init_timeout}
	    end;
	_ -> ok
    end.

server_init(Pid) ->
    register(?SERVER_PROC, self()),
    Pid ! {?SERVER_PROC, ok},
    server_loop([]).

server_loop(Tabs) ->
    receive
	quit ->
	    lists:foreach(fun(T) -> ets:delete(T) end, Tabs);
	{APid, Ref, {file2tab, File}} ->
	    case catch ets:file2tab(File) of
		{'EXIT', R} -> 
		    APid ! {Ref, {error, R}},
		    server_loop(Tabs);
		{ok, Tab} -> 
		    APid ! {Ref, {ok, Tab}},
		    server_loop([Tab|(Tabs--[Tab])])
	    end;
	{APid, Ref, {new, {Tab, Opts}}} ->
	    case lists:member(Tab, Tabs) of
		false ->
		    case catch ets:new(Tab, Opts++[named_table,public]) of
			{'EXIT', R} -> 
			    APid ! {Ref, {error, R}},
			    server_loop(Tabs);
			T -> 
			    APid ! {Ref, T},
			    server_loop([Tab|Tabs])
		    end;
		true -> 
		    APid ! {Ref, {error,already_exists}},
		    server_loop(Tabs)
	    end;
	{APid, Ref, {delete, Tab}} ->
	    case lists:member(Tab, Tabs) of
		true -> 
		    case catch ets:delete(Tab) of
			{'EXIT', R} ->
			    APid ! {Ref, {delete_error, R}},
			    server_loop(Tabs);
			_ ->
			    APid ! {Ref, ok},
			    server_loop(Tabs--[Tab])
		    end;
		false ->
		    APid ! {Ref, {error,doesnt_exist}},
		    server_loop(Tabs)
	    end;
	{APid, Ref, {info,_}} ->
	    APid ! {Ref, {ok, Tabs}},
	    server_loop(Tabs);
	info ->
	    lists:foreach(fun(T) -> io:fwrite("~p~n", [T]) end, Tabs),
	    server_loop(Tabs);
	{APid, Ref, X} -> 
	    ok = io:fwrite("~p: bad msg: ~p~n", [?MODULE, X]),
	    APid ! {Ref, {error, bad_cmd}},
	    server_loop(Tabs);
	X ->
	    ok = io:fwrite("~p: very bad msg: ~p~n", [?MODULE, X]),
	    server_loop(Tabs)
    end.

