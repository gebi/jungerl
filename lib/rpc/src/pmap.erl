%%% File    : pmap.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : Port mapper interface
%%% Created : 14 Aug 1997 by Tony Rogvall <tony@erix.ericsson.se>
%%% Copyright (c) 2000 Sendmail, Inc.  All rights reserved.

-module(pmap).
-compile([verbose, report_errors, report_warnings, trace]).
-author('tony@erix.ericsson.se').

-export([open/1, open/2, close/1]).
-export([null/1, getport/4, set/5, unset/5, dump/1, callit/5, dump_fmt/1]).


-include("pmap.hrl").

open(Host) -> open(Host, tcp).

open(Host, Proto) -> 
    rpc_client:open(Host, ?PMAP_PROG, ?PMAP_VERS, Proto, ?PMAP_PORT).

close(Clnt) ->
    rpc_client:close(Clnt).

%%
%%
%%

null(Clnt) ->
    pmap_clnt:pmapproc_null_2(Clnt).

getport(Clnt, Prog, Vers, Proto) ->
    NProto = proto(Proto),
    case pmap_clnt:pmapproc_getport_2(Clnt,{Prog, Vers, NProto,0}) of
	{ok, 0} ->
	    {error, 'EPROGUNAVAIL'};
	{ok, N} ->
	    {ok, N};
	Error ->
	    Error
    end.

set(Clnt,Prog,Vers,Proto,Port) -> 
    NProto = proto(Proto),
    pmap_clnt:pmapproc_set_2(Clnt,{Prog, Vers, NProto, Port}).

unset(Clnt, Prog,Vers,Proto,Port) ->
    NProto = proto(Proto),
    pmap_clnt:pmapproc_unset_2(Clnt,{Prog, Vers, NProto, Port}).

dump(Clnt) ->
    case pmap_clnt:pmapproc_dump_2(Clnt) of
	{ok, Map} -> {ok, dump_list(Map)};
	Error -> Error
    end.

callit(Clnt, Prog, Vers, Proc, Args) ->
    pmap_clnt:pmapproc_callit_2(Clnt, {Prog,Vers,Proc,Args}).


dump_fmt(Clnt) ->
    case dump(Clnt) of
	{ok, List} -> fmt_list(List);
	Error -> Error
    end.

fmt_list(List) ->
    io:format(" ~-15s ~-10s ~-5s ~-5s~n", 
	      ["program", "version", "proto", "port"]),
    io:format(" ~s~n", [lists:duplicate(38,$-)]),
    lists:foreach(
      fun({Prog,Ver,Proto,Port}) ->
	      io:format(" ~-15s ~-10w ~-5s ~-5w~n", 
			[prog(Prog), Ver, Proto, Port])
      end, List).

dump_list({Mapping, Next}) ->
    case Mapping of
	{Prog,Ver,?IPPROTO_TCP,Port} ->
	    [ {Prog,Ver,tcp,Port} | dump_list(Next) ];
	{Prog,Ver,?IPPROTO_UDP,Port} ->
	    [ {Prog,Ver,udp,Port} | dump_list(Next) ];
	_ ->
	    [Mapping | dump_list(Next)]
    end;
dump_list(void) -> 
    [].


proto(tcp) -> ?IPPROTO_TCP;
proto(udp) -> ?IPPROTO_UDP.

%% program number to program name (for standard names)
prog(100000) -> "PMAP";
prog(100003) -> "NFS";
prog(100026) -> "BOOTPARAM";
prog(100020) -> "KLM";
prog(100005) -> "MOUNT";
prog(100021) -> "NLM";
prog(100017) -> "REX";
prog(100011) -> "RQUOTA";
prog(100001) -> "RSTAT";
prog(100002) -> "RUSERS";
prog(100024) -> "SM";
prog(100012) -> "SPRAY";
prog(100004) -> "YP";
prog(16#40000000) -> "YPPUSH_XFRRESP";
prog(100007) -> "YPBIND";
prog(100009) -> "YPPASSWD";
prog(100028) -> "YPU";
prog(X) ->  integer_to_list(X).




