%%% File    : ssh_epmd.erl
%%% Author  : Tony Rogvall <tony@bulldog>
%%% Description : EPMD over SSH
%%% Created : 30 Aug 2004 by Tony Rogvall <tony@bulldog>

-module(ssh_epmd).

-behaviour(gen_server).

%% External exports
-export([port_please/1, port_please/2, port_please/3, 
	 names/0, names/1]).

-import(lists, [reverse/1]).

-include_lib("kernel/src/inet_int.hrl").

-define(EPMD_ALIVE, $a).
-define(EPMD_PORT_PLEASE, $p).
-define(EPMD_NAMES, $n).
-define(EPMD_DUMP, $d).
-define(EPMD_KILL, $k).
-define(EPMD_STOP, $s).

-define(EPMD_ALIVE_OK, $Y).

-define(EPMD_ALIVE2_REQ, $x).
-define(EPMD_PORT_PLEASE2_REQ, $z).
-define(EPMD_ALIVE2_RESP, $y).
-define(EPMD_PORT2_RESP, $w).

%% Hmmm
-define(erlang_daemon_port, 4369).
-define(epmd_dist_low, 5).
-define(epmd_dist_high, 5).



%% Lookup a node "Name" at Host
%% return {port, P, Version} | noport
%%
port_please(Node) when atom(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
	[Name, "ssh:"++Host] ->
	    case ssh_cm:dist_start(Node) of
		{ok, CM} ->
		    ssh_cm:attach(CM),
		    get_port_cm(CM, Name);
		Error ->
		    Error
	    end;
	_ ->
	    {error, einval}
    end.

port_please(Name, Host) ->
    port_please(Name, Host, infinity).

port_please(Name, HostName, Timeout) when atom(HostName) ->
    port_please1(Name,atom_to_list(HostName), Timeout);
port_please(Name,HostName, Timeout) when list(HostName) ->
    port_please1(Name,HostName, Timeout);
port_please(Name, EpmdAddr, Timeout) ->
    get_port(Name, EpmdAddr, Timeout).


port_please1(Name,HostName, Timeout) ->
    case inet:gethostbyname(HostName, inet, Timeout) of
	{ok,{hostent, _Name, _ , _Af, _Size, [EpmdAddr | _]}} ->
	    get_port(Name, EpmdAddr, Timeout);
	Else ->
	    Else
    end.

names() ->
    erl_epmd:names().

names(HostName) when atom(HostName) ->
  names1(atom_to_list(HostName));
names(HostName) when list(HostName) ->
  names1(HostName);
names(EpmdAddr) ->
  get_names(EpmdAddr).

names1(HostName) ->
    case inet:gethostbyname(HostName) of
	{ok,{hostent, _Name, _ , _Af, _Size, [EpmdAddr | _]}} ->
	    get_names(EpmdAddr);
	Else ->
	    Else
    end.


epmd_dist_high() ->
    case os:getenv("ERL_EPMD_DIST_HIGH") of
	false ->
	    ?epmd_dist_high; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when integer(N), N < ?epmd_dist_high ->
		    N;
		_ ->
		    ?epmd_dist_high
	    end
    end.

epmd_dist_low() ->
    case os:getenv("ERL_EPMD_DIST_LOW") of
	false ->
	   ?epmd_dist_low; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when integer(N), N > ?epmd_dist_low ->
		    N;
		_ ->
		   ?epmd_dist_low
	    end
    end.
		    


%%% (When we reply 'duplicate_name', it's because it's the most likely
%%% reason; there is no interpretation of the error result code.)
wait_for_reg_reply(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
	    case SoFar ++ Data0 of
		[$y, Result, A, B] ->
		    case Result of
			0 ->
			    {alive, Socket, ?u16(A, B)};
			_ ->
			    {error, duplicate_name}
		    end;
		Data when length(Data) < 4 ->
		    wait_for_reg_reply(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    {error, epmd_close}
    after 10000 ->
	    gen_tcp:close(Socket),
	    {error, no_reg_reply_from_epmd}
    end.
    
wait_for_reg_reply_v0(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
	    case SoFar ++ Data0 of
		[$Y, A, B] ->
		    {alive, Socket, ?u16(A, B)};
		Data when length(Data) < 3 ->
		    wait_for_reg_reply(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    {error, duplicate_name}		% A guess -- the most likely reason.
    after 10000 ->
	    gen_tcp:close(Socket),
	    {error, no_reg_reply_from_epmd}
    end.


%%% Not used anymore
%%% get_port(Node, EpmdAddress) ->
%%%     get_port(Node, EpmdAddress, infinity).

get_port(Name, EpmdAddress, Timeout) ->
    case ssh_cm:start(EpmdAddress, []) of
	{ok, CM} ->
	    get_port_cm(CM, Name);
	Error ->
	    Error
    end.

get_port_cm(CM, Name0) ->
    case ssh_cm:direct_tcpip(CM, {127,0,0,1}, ?erlang_daemon_port,
			     {127,0,0,1}, ?erlang_daemon_port) of
	{ok, Channel} ->
	    Name = to_string(Name0),
	    Len = 1+length(Name),
	    ssh_cm:send(CM, Channel,[?int16(Len),?EPMD_PORT_PLEASE2_REQ,Name]),
	    Reply = wait_for_port_reply(CM, Channel, []),
	    case Reply of
		closed ->
		    get_port_v0(CM, Name0);
		Other ->
		    Other
	    end;
	_Error -> 
	    noport
    end.

%%
%% Lookup a node "Name" at Host
%%
get_port_v0(CM, Name0) ->
    case ssh_cm:direct_tcpip(CM, {127,0,0,1}, ?erlang_daemon_port,
			     {127,0,0,1}, ?erlang_daemon_port) of
	{ok, Channel} ->
	    Name = cstring(Name0),
	    Len = 1+length(Name),
	    ssh_cm:send(CM, Channel, [?int16(Len),?EPMD_PORT_PLEASE, Name]),
	    wait_for_port_reply_v0(CM, Channel, []);
	_Error -> 
	    noport
    end.


wait_for_port_reply_v0(CM, Channel, SoFar) ->
    receive
	{ssh_cm, CM, {data, Channel, 0, Data0}} ->
	    ssh_cm:adjust_window(CM, Channel, size(Data0)),
	    case SoFar ++ binary_to_list(Data0) of
		[A,B] ->
		    wait_for_close(CM, Channel, {port, ?u16(A, B), 0});
		Data when length(Data) < 2 ->
		    wait_for_port_reply_v0(CM, Channel, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{ssh_cm, CM, {closed, Channel}} ->
	    noport;
	{ssh_cm, CM, {eof, Channel}} ->
	    ssh_cm:close(CM, Channel),
	    noport
    after 10000 ->
	    ssh_cm:close(CM, Channel),
	    noport
    end.

wait_for_port_reply(CM, Channel, SoFar) ->
    receive
	{ssh_cm, CM, {data, Channel, 0, Data0}} ->
	    ssh_cm:adjust_window(CM, Channel, size(Data0)),
	    case SoFar ++ binary_to_list(Data0) of
		[$w, Result | Rest] ->
		    case Result of
			0 ->
			    wait_for_port_reply_cont(CM,Channel,Rest);
			_ ->
			    wait_for_close(CM, Channel, noport)
		    end;
		Data when length(Data) < 2 ->
		    wait_for_port_reply(CM, Channel, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{ssh_cm, CM, {closed, Channel}} ->
	    noport;
	{ssh_cm, CM, {eof, Channel}} ->
	    ssh_cm:close(CM, Channel),
	    noport
    after 10000 ->
	    ssh_cm:close(CM, Channel),
	    noport
    end.


wait_for_port_reply_cont(CM, Channel, SoFar) when length(SoFar) >= 10 ->
    wait_for_port_reply_cont2(CM, Channel, SoFar);
wait_for_port_reply_cont(CM, Channel, SoFar) ->
    receive
	{ssh_cm, CM, {data, Channel, 0, Data0}} ->
	    ssh_cm:adjust_window(CM, Channel, size(Data0)),
	    case SoFar ++ binary_to_list(Data0) of
		Data when length(Data) >= 10 ->
		    wait_for_port_reply_cont2(CM, Channel, Data);
		Data when length(Data) < 10 ->
		    wait_for_port_reply_cont(CM, Channel, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{ssh_cm, CM, {closed, Channel}} ->
	    noport;
	{ssh_cm, CM, {eof, Channel}} ->
	    ssh_cm:close(CM, Channel),
	    noport
    after 10000 ->
	    ssh_cm:close(CM, Channel),
	    noport
    end.

wait_for_port_reply_cont2(CM, Channel, Data) ->
    [A, B, _Type, _Proto, HighA, HighB,
     LowA, LowB, NLenA, NLenB | Rest] = Data,
    wait_for_port_reply_name(CM, Channel, ?u16(NLenA, NLenB), Rest),
    Low = ?u16(LowA, LowB),
    High = ?u16(HighA, HighB),
    Version = best_version(Low, High),
    {port, ?u16(A, B), Version}.

%%% Throw away the rest of the message; we won't use any of it anyway,
%%% currently.
wait_for_port_reply_name(CM, Channel, Len, Sofar) ->
    receive
	{ssh_cm, CM, {data, Channel, 0, Data0}} ->
	    ssh_cm:adjust_window(CM, Channel, size(Data0)),
	    wait_for_port_reply_name(CM, Channel, Len, Sofar);	    
	{ssh_cm, CM, {closed, Channel}} ->
	    closed;
	{ssh_cm, CM, {eof, Channel}} ->
	    ssh_cm:close(CM, Channel),
	    closed
    end.
		    

best_version(Low, High) ->
    OurLow =  epmd_dist_low(),
    OurHigh =  epmd_dist_high(),
    select_best_version(OurLow, OurHigh, Low, High).

%%% We silently assume that the low's are not greater than the high's.
%%% We should report if the intervals don't overlap.
select_best_version(L1, _H1, _L2, H2) when L1 > H2 ->
    0;
select_best_version(_L1, H1, L2, _H2) when L2 > H1 ->
    0;
select_best_version(_L1, H1, L2, _H2) when L2 > H1 ->
    0;
select_best_version(_L1, H1, _L2, H2) ->
    min(H1, H2).

min(A, B) when A < B ->
    A;
min(_A, B) ->
    B.

wait_for_close(CM, Channel, Reply) ->
    receive
	{ssh_cm, CM, {closed, Channel}} ->
	    Reply;
	{ssh_cm, CM, {eof, Channel}} ->
	    ssh_cm:close(CM, Channel),
	    Reply
    after 10000 ->
	    ssh_cm:close(CM, Channel),
	    Reply
    end.


%%
%% Creates a (flat) null terminated string from atom or list.
%%
cstring(S) when atom(S) -> cstring(atom_to_list(S));
cstring(S) when list(S) -> S ++ [0].

to_string(S) when atom(S) -> atom_to_list(S);
to_string(S) when list(S) -> S.

%%
%% Find names on epmd
%%
%%
get_names(EpmdAddress) ->
    case open(EpmdAddress) of
	{ok, Socket} ->
	    do_get_names(Socket);
	_Error ->
	    {error, address}
    end.

do_get_names(Socket) ->
    gen_tcp:send(Socket, [?int16(1),?EPMD_NAMES]),
    receive
	{tcp, Socket, [P0,P1,P2,P3|T]} ->
	    EpmdPort = ?u32(P0,P1,P2,P3),
	    if EpmdPort == ?erlang_daemon_port ->
		    names_loop(Socket, T, []);
	       true ->
		    close(Socket),
		    {error, address}
	    end;
	{tcp_closed, Socket} ->
	    {ok, []}
    end.

names_loop(Socket, Acc, Ps) ->
    receive
	{tcp, Socket, Bytes} ->
	    {NAcc, NPs} = scan_names(Acc ++ Bytes, Ps),
	    names_loop(Socket, NAcc, NPs);
	{tcp_closed, Socket} ->
	    {_, NPs} = scan_names(Acc, Ps),
	    {ok, NPs}
    end.

scan_names(Buf, Ps) ->
    case scan_line(Buf, []) of
	{Line, NBuf} ->
	    case parse_line(Line) of
		{ok, Entry} -> 
		    scan_names(NBuf, [Entry | Ps]);
		error ->
		    scan_names(NBuf, Ps)
	    end;
	[] -> {Buf, Ps}
    end.


scan_line([$\n | Buf], Line) -> {reverse(Line), Buf};
scan_line([C | Buf], Line) -> scan_line(Buf, [C|Line]);
scan_line([], _) -> [].

parse_line("name " ++ Buf0) ->
    case parse_name(Buf0, []) of
	{Name, Buf1}  ->
	    case Buf1 of
		"at port " ++ Buf2 ->
		    case catch list_to_integer(Buf2) of
			{'EXIT', _} -> error;
			Port -> {ok, {Name, Port}}
		    end;
		_ -> error
	    end;
	error -> error
    end;
parse_line(_) -> error.


parse_name([$\s | Buf], Name) -> {reverse(Name), Buf};
parse_name([C | Buf], Name) -> parse_name(Buf, [C|Name]);
parse_name([], _Name) -> error.



open({A,B,C,D}=EpmdAddr) when ?ip(A,B,C,D) ->
    gen_tcp:connect(EpmdAddr, ?erlang_daemon_port, [inet|inet_tcp_dist:get_node_connect_opts()]);
open({A,B,C,D,E,F,G,H}=EpmdAddr) when ?ip6(A,B,C,D,E,F,G,H) ->
    gen_tcp:connect(EpmdAddr, ?erlang_daemon_port, [inet6]).

open({A,B,C,D}=EpmdAddr, Timeout) when ?ip(A,B,C,D) ->
    gen_tcp:connect(EpmdAddr, ?erlang_daemon_port, [inet|inet_tcp_dist:get_node_connect_opts()], Timeout);
open({A,B,C,D,E,F,G,H}=EpmdAddr, Timeout) when ?ip6(A,B,C,D,E,F,G,H) ->
    gen_tcp:connect(EpmdAddr, ?erlang_daemon_port, [inet6], Timeout).

close(Socket) ->
    gen_tcp:close(Socket).

