%%%-------------------------------------------------------------------
%%% File    : psocket.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : PF_PACKET socket interface via port program
%%%
%%% Created :  2 Dec 2003 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(psocket).

-compile(export_all).

%% Command constants.
-define(WRITE, 0).
-define(JOIN,  1).
-define(DROP,  2).

%% Start a packet-socket server listening on IFName (a string e.g. "eth0").
%% Returns: {ok, Port, MAC_Address:binary()}
%%
%% Normal port I/O operations ('receive' and e.g. port_command/2) are
%% used to receive and send ethernet frames.
open(IFName) ->
    P = open_port({spawn, filename:join(code:priv_dir(psocket), "psocket_srv " ++ IFName)},
		  [{packet, 2}, use_stdio, binary, eof]),
    link(P), %% what's this actually do? I don't get an EXIT from the port
    receive
	{P, {data, <<MAC:6/binary>>}} ->
	    {ok, P, MAC};
	{P, eof} ->
	    erlang:port_close(P),
	    {error, closed}
    end.

write(Port, Frame)          -> port_command(Port, <<?WRITE, Frame/binary>>).
join_multicast(Port, MAddr) -> port_command(Port, <<?JOIN, MAddr/binary>>).
drop_multicast(Port, MAddr) -> port_command(Port, <<?DROP, MAddr/binary>>).

