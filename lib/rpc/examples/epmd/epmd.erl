%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%%% File    : epmd.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : EPMD interface
%%% Created : 20 Aug 1997 by Tony Rogvall <tony@erix.ericsson.se>

-module(epmd).
-author('tony@erix.ericsson.se').

-export([clnt_open/1, clnt_open/2, clnt_close/1]).
-export([null/1, getport/4, set/5, unset/5, dump/1, dump_fmt/1]).

-include("epmd.hrl").


clnt_open(Host) ->
    clnt_open(Host, tcp).

clnt_open(Host, Proto) ->
    case pmap:clnt_open(Host) of
	{ok, Clnt} ->
	    case pmap:getport(Clnt, ?EPMD_PROG, ?EPMD_VERS, Proto) of
		{ok, Port} ->
		    clnt_open(Host, Port, Proto);
		Error ->
		    pmap:close(Clnt),
		    Error
	    end;
	Error -> Error
    end.

clnt_open(Host, Port, Proto) ->
    rpc_clnt:open(tcp, {Host,Port}, null).

clnt_close(Clnt) ->
    rpc_clnt:close(Clnt).

proto(tcp) -> ?IPPROTO_TCP;
proto(udp) -> ?IPPROTO_UDP.

null(Clnt) ->
    epmd_clnt:epmdproc_null(Clnt).

getport(Clnt, Prog, Vers, Proto) ->
    NProto = proto(Proto),
    epmd_clnt:epmdproc_getport_2(Clnt,{Prog, Vers, NProto,0}).

set(Clnt,Prog,Vers,Proto,Port) -> 
    NProto = proto(Proto),
    epmd_clnt:epmdproc_set_2(Clnt,{Prog, Vers, NProto, Port}).

unset(Clnt, Prog,Vers,Proto,Port) ->
    NProto = proto(Proto),
    epmd_clnt:epmdproc_unset_2(Clnt,{Prog, Vers, NProto, Port}).

dump(Clnt) ->
    case epmd_clnt:epmdproc_dump_2(Clnt) of
	{ok, Map} -> {ok, dump_list(Map)};
	Error -> Error
    end.

dump_fmt(Clnt) ->
    case dump(Clnt) of
	{ok, List} -> fmt_list(List);
	Error -> Error
    end.

fmt_list(List) ->
    io:format(" ~-10s -10s ~-10s ~-5s ~-5s~n", 
	      ["node", "service", "version", "proto", "port"]),
    io:format(" ~s~n", [lists:duplicate(33,$-)]),
    lists:foreach(
      fun({Node,Service,Ver,Proto,Port}) ->
	      io:format(" ~-10s ~-10s ~-10w ~-5s ~-5w~n", 
			[Node, Service, Ver, Proto, Port])
      end, List).

dump_list({Mapping, Next}) ->
    case Mapping of
	{Node, Service, Ver, ?IPPROTO_TCP, Port} ->
	    [ {Node, Service, Ver,tcp, Port} | dump_list(Next) ];
	{Node, Service, Ver,?IPPROTO_UDP, Port} ->
	    [ {Node, Service, Ver,udp,Port} | dump_list(Next) ];
	_ ->
	    [Mapping | dump_list(Next)]
    end;
dump_list(void) -> 
    [].






