%%%----------------------------------------------------------------------
%%% File    : eth2udp.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Ethernet-over-UDP tunnels
%%% Created : 18 Feb 2003 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(eth2udp).
-author('luke@bluetail.com').

-import(lists, [foreach/2]).

-export([start_link/2, start_link/3]).
-export([init/3]).
-export([loop/3]).

%% start_link(Port, [EndPoint]) -> pid()
%%
%% Port = integer() (UDP port for local endpoint)
%% EndPoint = {IP, Port} (UDP endpoint of remote tunnel)
start_link(Port, EPs) ->
    start_link(Port, EPs, undefined).
start_link(Port, EPs, Dev) ->
    spawn_link(?MODULE, init, [Port, EPs, Dev]).

init(Port, EPs, Dev) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    {ok, Tunnel} = init_tunnel(Dev),
    tuntap:set_active(Tunnel, true),
    loop(Tunnel, Socket, EPs).

loop(Tunnel, Socket, EPs) ->
    receive
	{Tunnel, {data, Packet}} ->
	    io:format("Got ~p byte packet on tap~n", [size(Packet)]),
	    foreach(fun({IP, Port}) ->
			    gen_udp:send(Socket, IP, Port, Packet)
		    end,
		    EPs);
	{udp, Socket, _, _, Packet} ->
	    io:format("Got ~p byte packet on UDP~n", [size(Packet)]),
	    tuntap:write(Tunnel, Packet)
    end,
    ?MODULE:loop(Tunnel, Socket, EPs).

init_tunnel(Dev) ->
    Tun = tuntap:open_tuntap(tap, Dev),
    Name = tuntap:device_name(Tun),
    io:format("Alive and kicking on ~p~n", [Name]),
    {ok, Tun}.

