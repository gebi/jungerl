%%%-------------------------------------------------------------------
%%% File    : hub.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Simple ethernet hub based on packet sockets.
%%%
%%% Created : 22 Jul 2004 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(hub).

-export([run/1]).
-import(lists, [map/2, foreach/2]).

%% Broadcast ethernet packets arriving on any network interface to all
%% other network interfaces, for a chosen set of interfaces.
%%
%% This is what an ethernet hub does.
%%
%% e.g. hub(["eth0", "eth1"]).
run(IfNames) ->
    loop(map(fun open_if/1, IfNames)).

loop(Ports) ->
    receive
	{Port, {data, Data}} ->
	    foreach(fun(P) -> send(P, Data) end, Ports -- [Port]);
	{Port, eof} ->
	    exit({port_closed, Port})
    end,
    loop(Ports).

%% open_if(IfName) -> port()
open_if(If) ->
    {ok, Port, _Mac} = psocket:open(If),
    Port.

send(Port, Data) -> psocket:write(Port, Data).
