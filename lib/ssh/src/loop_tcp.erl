%%% File    : loop_tcp.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Loop driver api
%%% Created : 16 Oct 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(loop_tcp).


%% Socket server for TCP/IP

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).


-include_lib("kernel/src/inet_int.hrl").

%% inet_tcp port lookup
getserv(Port) when integer(Port) -> {ok, Port};
getserv(Name) when atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_tcp address lookup
getaddr(Address) -> inet:getaddr(Address, inet).
getaddr(Address,Timer) -> inet:getaddr_tm(Address, inet, Timer).

%% inet_tcp address lookup
getaddrs(Address) -> inet:getaddrs(Address, inet).
getaddrs(Address,Timer) -> inet:getaddrs_tm(Address,inet,Timer).
    
%%
%% Send data on a socket
%%
send(Socket, Packet) -> 
    %% XXX This result mapping is a kludge, that should really be fixed
    %% in prim_inet, but I do not want to make a patch on ERTS right 
    %% now, which a change in prim_inet would imply. 
    %% / raimo
    case prim_inet:send(Socket, Packet) of
	{error, _} ->
	    {error, einval};
	Result ->
	    Result
    end.

%%
%% Receive data from a socket (inactive only)
%%
recv(Socket, Length) -> prim_inet:recv(Socket, Length).
recv(Socket, Length, Timeout) -> prim_inet:recv(Socket, Length, Timeout).

unrecv(Socket, Data) -> prim_inet:unrecv(Socket, Data).

%%
%% Shutdown one end of a socket
%%
shutdown(Socket, How) ->
    prim_inet:shutdown(Socket, How).
    
%%
%% Close a socket (async)
%%
close(Socket) -> 
    inet:tcp_close(Socket).

%%
%% Set controlling process
%%
controlling_process(Socket, NewOwner) ->
    inet:tcp_controlling_process(Socket, NewOwner). 

%%
%% Connect
%%
connect(Address, Port, Opts) ->
    do_connect(Address, Port, Opts, infinity).

connect(Address, Port, Opts, infinity) ->
    do_connect(Address, Port, Opts, infinity);
connect(Address, Port, Opts, Timeout) when integer(Timeout), Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect({A,B,C,D}, Port, Opts, Time) when ?ip(A,B,C,D), integer(Port) ->
    case inet:connect_options(Opts, inet) of
	{error, Reason} -> exit(Reason);
	{ok, R} ->
	    Fd       = R#connect_opts.fd,
	    BAddr    = R#connect_opts.ifaddr,
	    BPort    = R#connect_opts.port,
	    SockOpts = R#connect_opts.opts,
	    case open(BAddr,BPort,SockOpts,stream,inet,?MODULE) of
		{ok, S} ->
		    case prim_inet:connect(S, {A,B,C,D}, Port, Time) of
			ok    -> {ok,S};
			Error ->  prim_inet:close(S), Error
		    end;
		Error -> Error
	    end
    end.

%% 
%% Listen
%%
listen(Port, Opts) ->
    exit(bad_call).

%%
%% Accept
%%
accept(L)         -> 
    exit(bad_call).

	    
accept(L,Timeout) -> 
    exit(bad_call).
%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    exit(bad_call).


load_driver() ->
    erl_ddll:load_driver(code:priv_dir(ssh), loop_drv).


open(Addr, Port, Opts, Type, Family, Module) ->
    case open(Family) of
	{ok,S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    case prim_inet:bind(S, Addr, Port) of
			{ok, _} -> 
			    inet_db:register_socket(S, Module),
			    {ok,S};
			Error  -> prim_inet:close(S), Error
		    end;
		Error  -> prim_inet:close(S), Error
	    end;
	Error -> Error
    end.

open(inet) -> open1(?INET_AF_INET);
open(inet6) -> open1(?INET_AF_INET6).

open1(Family) ->
    ok = load_driver(),
    S = erlang:open_port({spawn,loop_drv}, [binary]),
    case ctl_cmd(S,?INET_REQ_OPEN,[Family]) of
	{ok, _} -> {ok,S};
	Error -> prim_inet:close(S), Error
    end.

%% Control command
ctl_cmd(Port, Cmd, Args) ->
    case catch port_control(Port, Cmd, Args) of
	[?INET_REP_OK | Reply]      -> {ok, Reply};
	[?INET_REP_ERROR| Err] -> {error, list_to_atom(Err)};
	{'EXIT', _} -> {error, einval};
	_ -> {error, internal}
    end.






