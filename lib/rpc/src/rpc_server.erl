%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%%%----------------------------------------------------------------------
%%% File    : rpc_server.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Purpose : Remote Procedure Call - Server process; registers with
%%%           port mapper, handles the sockets, dispatches to
%%%           server implementation code (possibly the generated _svc 
%%%           module).
%%%           Provides a similar API as the original version written by
%%%           Sendmail.  The implementation is quite different though.
%%%
%%%           There is one gen_server process for each rpc server with
%%%           a unique program number and version number combination.
%%%           The gen_server process spawns a TCP listen/accept process
%%%           which sets the gen_server as controlling_process for all
%%%           accepted sockets.  After doing this, it sets the socket
%%%           as {active, once}.  For UDP, the server itself opens the
%%%           socket, sets it as {active, once}.
%%%           The server can listen to tcp and udp at the same time.
%%%           The gen_server thus is not explicitly aware of which sockets
%%%           it receives data from.  When it gets data, it decodes the
%%%           RPC message, invokes a dispatch function, and sets the
%%%           socket {active, once} again.
%%% Created :  6 Nov 2000 by Martin Bjorklund <mbj@bluetail.com>
%%%----------------------------------------------------------------------

%%% TODO:  * different callback functions for different prog versions

-module(rpc_server).
-author('mbj@bluetail.com').

-behaviour(gen_server).

%% External exports
-export([start_link/7, start_link/8, client_ip/1, client_socket/1, reply/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Internal exports
-export([listener/2]).

-record(state, {prg_num,         % int()
		prg_vsns,        % [{int(), func_name}]
		mod,             % atom()
		state,           % term()
		udp,             % #sock_s | undefined
		tcp              % #sock_s | undefined
	       }).

-record(sock_s, {proto,   % tcp | udp
		 ip,
		 port,
		 pmap_p,  % bool(), true registers w/ port mapper
		 opts
		 }).

-record(client, {sock,
		 addr,  % {Ip, Port} for udp, 'sock' for tcp
		 xid,
		 rverf}).


-include("rpc.hrl").

-define(LOCALHOST_ADDR, {127, 0, 0, 1}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: start_link(...)
%%       start_link(Name, ...)
%% Types: Protos = [{tcp|udp, Ip, Port, Pmap_p, SockOpts}]
%%        SockOpts = list of extra options to gen_tcp:listen /
%%                   gen_udp:open (see listener() for the generic
%%                   options, which cannot be changed)
%%        Name = <see gen_server:start_link()>
%% Purpose: Starts a (possibly named) gen_server for the rpc 
%%          service.
%%-----------------------------------------------------------------
start_link(Protos, PrgNum, PrgName, PrgVsnLo, PrgVsnHi, Mod, InitArgs) ->
    S = init_state(Protos, PrgNum, PrgName, PrgVsnLo, PrgVsnHi, Mod, InitArgs),
    gen_server:start_link(?MODULE, [S], []).
start_link(Name, Protos, PrgNum, PrgName, PrgVsnLo, PrgVsnHi, Mod, InitArgs) ->
    S = init_state(Protos, PrgNum, PrgName, PrgVsnLo, PrgVsnHi, Mod, InitArgs),
    gen_server:start_link(Name, ?MODULE, [S], []).

init_state(Protos, PrgNum, PrgName, PrgVsnLo, PrgVsnHi, Mod, InitArgs)
  when integer(PrgNum), atom(PrgName), integer(PrgVsnLo), integer(PrgVsnHi),
       PrgVsnLo =< PrgVsnHi ->
    S = protos(Protos, #state{}),
    {ok, State} = apply(Mod, init, [InitArgs]),
    PrgVsns = lists:map(
		fun(V) ->
			{V, list_to_atom(atom_to_list(PrgName) ++ "_" ++ 
					 integer_to_list(V))}
		end, lists:seq(PrgVsnLo, PrgVsnHi)),
    S#state{prg_num = PrgNum,
	    prg_vsns = PrgVsns,
	    mod = Mod,
	    state = State}.
							      
protos([{Proto, Ip, Port, Pmap_p, SockOpts} | T], S) ->
    SockS = #sock_s{proto = Proto,
		    ip = Ip,
		    port = Port,
		    pmap_p = Pmap_p,
		    opts = SockOpts},
    if Proto == tcp -> protos(T, S#state{tcp = SockS});
       Proto == udp -> protos(T, S#state{udp = SockS});
       true -> exit({badarg, Proto})
    end;
protos([], S) ->
    S;
protos([H|_], _) ->
    exit({badarg, H}).
		 
%%-----------------------------------------------------------------
%% Func: client_ip(#client) -> {ok, {ip(), Port}} | {error, Reason}
%% Purpose: Returns the IP address and port of the client of this
%%          session.  Can be used by a server implementation to
%%          get info about a certain client.
%%          Ref is passed to the server implementation code in
%%          each call.
%%-----------------------------------------------------------------
client_ip(Clnt) when Clnt#client.addr == sock ->
    inet:peername(Clnt#client.sock);
client_ip(Clnt) ->
    {ok, Clnt#client.addr}.

%%-----------------------------------------------------------------
%% Func: client_socket(#client) -> Sock
%% Purpose: Returns the socket associated with a client.  Note that
%%          for UDP, all clients are multiplexed onto the same
%%          socket.
%%          Ref is passed to the server implementation code in
%%          each call.
%%-----------------------------------------------------------------
client_socket(Clnt) ->
    Clnt#client.sock.

%%-----------------------------------------------------------------
%% Func: reply(Clnt, Status, Bytes) -> void
%% Types: Clnt = #client
%%        Status = success | garbage_args | error
%%        Bytes = io_list of bytes, IFF Status == success
%% Purpose: Send a rpc reply back to the client.  MUST only be
%%          called if the callback module returned {noreply, S'}
%%          on the original call.
%%-----------------------------------------------------------------
reply(Clnt, Status, Bytes) ->
    do_reply(Status, Bytes, Clnt).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, S}          |
%%          {ok, S, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([S]) ->
    process_flag(trap_exit, true),
    create_socks(S),
    {ok, S}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, S}          |
%%          {reply, Reply, S, Timeout} |
%%          {noreply, S}               |
%%          {noreply, S, Timeout}      |
%%          {stop, Reason, Reply, S}   | (terminate/2 is called)
%%          {stop, Reason, S}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, S) ->
    Res = apply(S#state.mod, handle_call, [Request, From, S#state.state]),
    mk_gen_return(Res, S).

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, S}          |
%%          {noreply, S, Timeout} |
%%          {stop, Reason, S}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, S) ->
    Res = apply(S#state.mod, handle_cast, [Msg, S#state.state]),
    mk_gen_return(Res, S).

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, S}          |
%%          {noreply, S, Timeout} |
%%          {stop, Reason, S}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({tcp, Sock, Data}, S) ->
    %% First, get the Record Marking 32-bit header
    <<Last:1/integer, _Len:31/integer, RpcMsg/binary>> = Data,
    Last = 1, % FIXME: for now...
    S1 = handle_msg(RpcMsg, Sock, sock, S),
    inet:setopts(Sock, [{active, once}]),
    {noreply, S1};

handle_info({udp, Sock, Ip, Port, Data}, S) ->
    S1 = handle_msg(Data, Sock, {Ip, Port}, S),
    inet:setopts(Sock, [{active, once}]),
    {noreply, S1};    

handle_info(Msg, S) ->
    Res = apply(S#state.mod, handle_info, [Msg, S#state.state]),
    mk_gen_return(Res, S).

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, S) ->
    unregister_with_portmapper(S#state.tcp, S),
    unregister_with_portmapper(S#state.udp, S),
    apply(S#state.mod, terminate, [Reason, S#state.state]).    

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
handle_msg(Msg, Sock, Addr, S) ->
    case catch handle_msg1(Msg, Sock, Addr, S) of
	{accepted, {Status, Bytes, NState}, Clnt} ->
	    do_reply(Status, Bytes, Clnt),
	    S#state{state = NState};
	{rejected, Clnt, RejectBody, NState} ->
	    Reply = {Clnt#client.xid, {'REPLY', {'MSG_DENIED', RejectBody}}},
	    send_reply(Clnt, rpc_xdr:enc_rpc_msg(Reply)),
	    S#state{state = NState};
	{noreply, NState} -> % the callback replies later
	    S#state{state = NState};
	{'EXIT', Reason} ->
	    log_error(S, {rpc_msg, Reason}),
	    S
    end.

%% The call to this function is catched, which means that bad
%% rpc messages and programming errors are handled the same way,
%% both end up in the error log.
handle_msg1(Msg, Sock, Addr, S) ->
    {{Xid, Body}, Off} = rpc_xdr:dec_rpc_msg(Msg, 0),
    {'CALL', {RpcVsn, Prg, Vsn, Proc, Cred, Verf}} = Body,
    Clnt0 = #client{sock = Sock, addr = Addr, xid = Xid},
    chk_rpc_vsn(RpcVsn, Clnt0),
    Clnt1 = chk_auth(Cred, Verf, Clnt0),
    Fun = chk_prg(Prg, Vsn, S, Clnt1),
    %% Ok, we're ready to call the implementation
    case (catch apply(S#state.mod, Fun, [Proc,Msg,Off,Clnt1,S#state.state])) of
	X = {success, Bytes, NState} ->
	    {accepted, X, Clnt1};
	{garbage_args, NState} ->
	    {accepted, {garbage_args, [], NState}, Clnt1};
	{noreply, NState} ->
	    {noreply, NState};
	{error, NState} ->
	    {accepted, {error, [], NState}, Clnt1};
	{'EXIT', Reason} ->
	    log_error(S, {S#state.mod, Reason}),
	    {accepted, {error, [], S#state.state}, Clnt1}
    end.
    
do_reply(success, Bytes, Clnt) ->
    Reply = accepted(Clnt, {'SUCCESS', <<>>}),
    send_reply(Clnt, [rpc_xdr:enc_rpc_msg(Reply), Bytes]);
do_reply(garbage_args, _, Clnt) ->
    Reply = accepted(Clnt, {'GARBAGE_ARGS', void}),
    send_reply(Clnt, rpc_xdr:enc_rpc_msg(Reply));
do_reply(error, _, Clnt) ->
    Reply = accepted(Clnt, {'SYSTEM_ERR', void}),
    send_reply(Clnt, rpc_xdr:enc_rpc_msg(Reply)).
    
accepted(C, AcceptBody) ->
    {C#client.xid, {'REPLY', {'MSG_ACCEPTED', {C#client.rverf, AcceptBody}}}}.
    


%% we support RPC version 2 only.
chk_rpc_vsn(?RPC_VERSION_2, _Clnt) -> ok;
chk_rpc_vsn(_, Clnt) -> throw({rejected, Clnt,
			      {'RPC_MISMATCH',
			       {?RPC_VERSION_2, ?RPC_VERSION_2}}}).

%% we should implement a more flexible authentication scheme...
chk_auth({'AUTH_NONE', _}, {'AUTH_NONE', _}, Clnt) ->
    Clnt#client{rverf = {'AUTH_NONE', <<>>}};
chk_auth({'AUTH_SYS', _}, {'AUTH_NONE', _}, Clnt) ->
    Clnt#client{rverf = {'AUTH_NONE', <<>>}};
chk_auth(_,_, Clnt) -> throw({rejected, Clnt, {'AUTH_ERROR', 'AUTH_TOOWEAK'}}).

chk_prg(Prg, Vsn, S, Clnt) ->
    if
	Prg /= S#state.prg_num ->
	    throw({accepted, Clnt, {'PROG_UNAVAIL', void}});
	true ->
	    case lists:keysearch(Vsn, 1, S#state.prg_vsns) of
		{value, {_, Fun}} -> Fun;
		_ ->
		    throw({accepted, Clnt,
			   {'PROG_MISMATCH', 
			    element(1, hd(S#state.prg_vsns)),
			    element(1, lists:last(S#state.prg_vsns))}})
	    end;
	true ->
	    ok
    end.
    
send_reply(Clnt, Reply) when Clnt#client.addr == sock ->
    Len = io_list_len(Reply),
    gen_tcp:send(Clnt#client.sock,
		 [<<1:1/integer, Len:31/unsigned-integer>>, Reply]);
send_reply(#client{sock = S, addr = {Ip, Port}}, Reply) ->
    gen_udp:send(S, Ip, Port, Reply).

%%-----------------------------------------------------------------
%% Create Socket endpoints; for tcp and udp.  The sockets
%% are transparent to the Parent.
%%-----------------------------------------------------------------
create_socks(S) ->
    {ok, Tcp} = proc_lib:start_link(?MODULE, listener, [self(), S]),
    {ok, Udp} = udp_open(S),
    S#state{tcp = Tcp,
	    udp = Udp}.

%%-----------------------------------------------------------------
%% The listen/accept process for TCP.
%%-----------------------------------------------------------------
listener(Parent, #state{tcp = undefined}) -> 
    proc_lib:init_ack(Parent, {ok, undefined});
listener(Parent, S) ->
    Tcp = S#state.tcp,
    Opts = [{mode, binary}, 
	    {ip, Tcp#sock_s.ip},
	    {reuseaddr, true},
	    {active, false},
	    {packet, sunrm},
	    {nodelay, true} | Tcp#sock_s.opts],
    {ok, LSock} = gen_tcp:listen(Tcp#sock_s.port, Opts),
    {ok, Port} = inet:port(LSock),
    Tcp1 = Tcp#sock_s{port = Port},
    register_with_portmapper(Tcp1, S),
    proc_lib:init_ack(Parent, {ok, Tcp1}),
    accept_loop(LSock, Parent).

accept_loop(LSock, Parent) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    gen_tcp:controlling_process(Sock, Parent),
    Parent ! {tcp_new, Sock},
    inet:setopts(Sock, [{active, once}]),
    accept_loop(LSock, Parent).


udp_open(#state{udp = undefined}) -> {ok, undefined};
udp_open(S) ->
    Udp = S#state.udp,
    Opts = [{mode, binary}, 
	    {ip, Udp#sock_s.ip},
	    {reuseaddr, true},
	    {active, false} | Udp#sock_s.opts],
    {ok, Sock} = gen_udp:open(Udp#sock_s.port, Opts),
    inet:setopts(Sock, [{active, once}]),
    {ok, Port} = inet:port(Sock),
    Udp1 = Udp#sock_s{port = Port},
    register_with_portmapper(Udp1, S),
    {ok, Udp1}.


register_with_portmapper(SockS, S) ->
    pmap_reg(SockS, S, set).

unregister_with_portmapper(SockS, S) ->
    pmap_reg(SockS, S, unset).

pmap_reg(undefined, S, _Func) -> ok;
pmap_reg(#sock_s{pmap_p = false}, S, _Func) -> ok;
pmap_reg(#sock_s{port = Port, proto = Proto},
	 #state{prg_num = Prg, prg_vsns = Vsns},
	 Func) ->
    {ok, PClnt} = pmap:open(?LOCALHOST_ADDR),
    lists:foreach(fun({Vsn, _Fun}) ->
			  case pmap:Func(PClnt, Prg, Vsn, Proto, Port) of
			      {ok, true} ->
				  pmap:close(PClnt);
			      {ok, false} ->
				  pmap:close(PClnt),
				  exit(pmap_reg)
			  end
		  end, Vsns).

log_error(S, Term) ->
    error_logger:format("rpc_server: prog:~p vsns:~p ~p\n", 
			[S#state.prg_num,
			 S#state.prg_vsns,
			 Term]).

io_list_len(L) -> io_list_len(L, 0).
io_list_len([H|T], N) ->
    if
	H >= 0, H =< 255 -> io_list_len(T, N+1);
	list(H) -> io_list_len(T, io_list_len(H,N));
	binary(H) -> io_list_len(T, size(H) + N);
	true -> exit({xdr, opaque})
    end;
io_list_len(H, N) when binary(H) ->
    size(H) + N;
io_list_len([], N) ->
    N.

mk_gen_return({reply, R, NState}, S) ->
    {reply, R, S#state{state = NState}};
mk_gen_return({noreply, NState}, S) ->
    {noreply, S#state{state = NState}};
mk_gen_return({reply, R, NState, Timeout}, S) ->
    {reply, R, S#state{state = NState}, Timeout};
mk_gen_return({noreply, NState, Timeout}, S) ->
    {noreply, S#state{state = NState}, Timeout};
mk_gen_return({stop, Reason, R, NState}, S) ->
    {stop, Reason, R, S#state{state = NState}};
mk_gen_return({stop, Reason, NState}, S) ->
    {stop, Reason, S#state{state = NState}}.
