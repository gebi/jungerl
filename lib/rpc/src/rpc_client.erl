%%% Copyright (c) 2000 Sendmail, Inc.  All rights reserved.

%%% ONC/RPC client-side server: takes RPC requests with XDR-encoded
%%% arguments, transmits them over the network, and receives the reply.

-module(rpc_client).
-behaviour(gen_server).

%%% API
-export([open/4, open/5, close/1, call/3, call/4]).
-export([call_bulk/4, call_bulk/5]).
-export([control/2, control/3,
	 get_xid/1,  set_xid/2, 
	 get_vers/1,
	 get_retry_timeout/1, set_retry_timeout/2,
	 get_timeout/1, set_timeout/2,
	 get_queue_limit/1, set_queue_limit/2,
	 get_socket/1, set_auth/2]).
-export([get_queue_length/1, controlling_process/2]).
-export([get_stats/1, get_stats/2,
         get_and_reset_stats/1, get_and_reset_stats/2]).

%%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

-include("rpc.hrl").

-define(DEFAULT_TIMEOUT, 60000).	% In milliseconds

%%% Note on state structure(s): the state can get quite large and may be
%%% updated frequently.  Because Erlang is functional, updates are made by
%%% creating a new copy of the record.  If heavy use causes frequent GC's,
%%% we can mimize the problem by splitting out the mostly-static state elements
%%% into a substructure.  No need to do this until it becomes a problem.

%%% The client responds to heavy load or failed servers through two mechanisms.
%%% Once the pending RPC request queue becomes too long, the client will refuse
%%% any new requests.  If any request's latency is greater than the timeout
%%% threshold, then that request fails.  If a request times out *and* the queue
%%% is full, then the entire client is shut down, terminating all pending
%%% requests.

%%% The pending request queue is stored as a list, and is managed with
%%% lists:keysearch() and lists:keydelete().  We could investigate
%%% storing the pending queue in an ETS table, but only when measurements
%%% show that a list is not adequate.

-record(state, {
	  xid = 0,              % next transaction id
	  timeout = ?DEFAULT_TIMEOUT, % current timeout (in milliseconds)
	  retry_timeout = 2000,	% retry timeout (in milliseconds)
	  program,              % RPC service program number
	  version,              % RPC service version number
	  auth,                 % authentication paramters
	  proto,                % protocol (tcp or udp)
	  ip,                   % ip address of server
	  port,                 % port on server
	  socket,               % socket handle
	  tcp_blocks = [],	% to hold partial replies over TCP
	  pending = [],		% list of PendingRecord
	  pendinglen = 0,	% length of pending list
	  pendingmax = 100,	% maximum number of pending RPC requests
	  call_const,	        % {HeadBin, AuthBin, Sz} constant fields in msg
	  stopping = false,	% flag: has stop() been called w/pending reqs?
	  statistics		% runtime statistics, see substructure
	 }).

%% A pending RPC call.
-record(pending, {
	xid,                    % for the packet
	from,			% as given to handle_call(),
	start_time,		% time of transmission, as returned from now()
	packet = null,		% the transmitted packet (udp) or null (tcp)
	timers = [],            % list of timers for this call
	bytes_out = 0,		% number of bytes transmitted
	bytes_in = 0		% number of bytes received
	}).

%% Operational statistics.
-record(statistics, {
	  %% Statistics gathered since this time, now() format
	  start_time,
	  %% Number of RPC events of various types
	  calls = 0,            % number of sent requests (no rxtmits)
	  replies = 0,          % number of received replies
	  timedout = 0,         % number of calls timedout
	  invalid = 0,
	  retries = 0,
	  %% Cumulative throughput
	  bytes_in = 0,
	  bytes_out = 0,
	  %% Latency statistics
	  lat_sumt = 0,			% sum of latencies (in microseconds)
	  lat_sumt2 = 0,		% sum of squares of latencies
					% (in microsec^2)
	  %% Procedure number histogram
	  proccounts_len = 0,		% size of 'proccounts' tuple
	  proccounts = {}		% per-op counts, indexed by call #
	 }).

%% Client interface

%% Create RPC client using portmapper to determine port number.
%%	Host: IPv4 address, host name, or fully qualified hostname
%%	 Program: program number of RPC service desired
%%	 Version: version number of RPC service desired
%%	 Proto: tcp | udp
%% Returns: {ok, Client} or {error, Reason}
%%          Reason = {Where, InetErrorAtom} | term()
%% NOTE: Client is a linked Pid.

open(Host, Program, Version, Proto) ->
    case pmap:open(Host) of
	{ok, Clnt} ->
	    Res = pmap:getport(Clnt, Program, Version, Proto),
	    pmap:close(Clnt),
	    case Res of
		{ok, Port} ->
		    open(Host,Program,Version,Proto,Port);
		Error -> Error
	    end;
	Error -> Error
    end.

%% Create RPC client using explicit port number.
%%	Host: IPv4 address, host name, or fully qualified hostname
%%	 Program: program number of RPC service desired
%%	 Version: version number of RPC service desired
%%	 Proto: tcp | udp
%% Returns: as open/4

open(Host, Program, Version, Proto, Port) ->
    gen_server:start_link(?MODULE, [Host, Program, Version, Proto, Port], []).

%% Soft close of RPC Client - allows pending requests to wait for replies.
%% Returns: ok

close(Clnt) -> 
    gen_server:call(Clnt, close, infinity).

%% Make and RPC call and wait for the reply.
%%	Clnt: the rpc_client PID
%%	Proc: the procedure number (within the rpc_clnt's program and version)
%%	Params: XDR-encoded arguments (as an I/O list)
%% Returns: {ok, Results} or {error, Reason}, where
%%	Results: XDR-encoded binary of the RPC reply

call(Clnt, Proc, Params) ->
    call(Clnt, Proc, Params, undefined).
call(Clnt, Proc, Params, TimeOut) ->
    gen_server:call(Clnt, {call, Proc, Params, TimeOut}, infinity).

call_bulk(Clnt, Proc, Params, BulkData) ->
    call_bulk(Clnt, Proc, Params, BulkData, undefined).
call_bulk(Clnt, Proc, Params, BulkData, TimeOut) ->
    gen_server:call(Clnt, {call_bulk, Proc, Params, BulkData, TimeOut},
		    infinity).

get_xid(Clnt)        -> control(Clnt, get_xid).
set_xid(Clnt, Xid)   -> control(Clnt, set_xid, Xid).

get_vers(Clnt)       -> control(Clnt, get_vers).
get_socket(Clnt)        -> control(Clnt, get_socket).

%% Time is in seconds
get_timeout(Clnt)       -> control(Clnt, get_timeout).
set_timeout(Clnt, Time) -> control(Clnt, set_timeout, Time).

get_queue_limit(Clnt)    -> control(Clnt, get_queue_limit).
set_queue_limit(Clnt, N) -> control(Clnt, set_queue_limit, N).

%% Time is in seconds
get_retry_timeout(Clnt)       -> control(Clnt, get_retry_timeout).
set_retry_timeout(Clnt, Time) -> control(Clnt, set_retry_timeout, Time).

set_auth(Clnt, Auth)	-> control(Clnt, set_auth, Auth).
get_queue_length(Clnt) -> control(Clnt, get_queue_length).

control(Clnt, Attr, Param) when atom(Attr) ->
    gen_server:call(Clnt, {Attr, Param}, infinity).

control(Clnt, Attr) when atom(Attr) ->
    gen_server:call(Clnt, Attr, infinity).

get_stats(Clnt) -> gen_server:call(Clnt, get_stats, infinity).
get_stats(Clnt, Timeout) -> gen_server:call(Clnt, get_stats, Timeout).

get_and_reset_stats(Clnt) ->
    gen_server:call(Clnt, get_and_reset_stats, infinity).
get_and_reset_stats(Clnt, Timeout) ->
    gen_server:call(Clnt, get_and_reset_stats, Timeout).

%%% Cause client process to unlink from caller and link to NewOwner.
controlling_process(Clnt, NewOwner) when pid(NewOwner) ->
    gen_server:call(Clnt, {set_owner, NewOwner}, infinity).

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([Host, Program, Version, Proto, Port]) ->
    Stats = #statistics{ start_time = now() },
    None = {'AUTH_NONE', <<>>},
    S0 = #state{ port = Port, program = Program, version = Version,
		 proto = Proto, statistics = Stats,
		 auth = {None, None} },
    S1 = call_const(S0),
    case inet:getaddr(Host, inet) of
	{ok, IP} when integer(Port) ->
	    case Proto of
		tcp ->
		    case gen_tcp:connect(IP, Port,[binary,{packet,sunrm}]) of
			{ok, Sock} ->
			    {ok, S1#state{ip = IP, socket = Sock}};
			{error, R} ->
			    {stop, {connect, R}}
		    end;
		udp ->
		    {ok, Sock} = gen_udp:open(0, [binary]),
		    ok = gen_udp:connect(Sock, IP, Port),
		    {ok, S1#state{ip = IP, socket = Sock}};
		_ ->
		    {stop, bad_proto}
	    end;
	{error, R} ->
	    {stop, {getaddr, R}}
    end.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

terminate(Reason, #state{proto = Proto, socket=Socket,
			 pending=Pending, pendinglen = PendingLen}) ->
    F = fun (#pending{from=From}) ->
		gen_server:reply(From, {error, Reason})
	end,
    lists:foreach(F, Pending),
    %% Because we're exiting, we won't bother updating #state.pendinglen.
    case Proto of
	udp -> gen_udp:close(Socket);
	tcp -> gen_tcp:close(Socket)
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({call,Procedure,Params,Timeout}, From, State) ->
    if
	State#state.pendinglen >= State#state.pendingmax ->
	    {reply, {error, queue_full}, State};
	State#state.stopping == true ->
	    {reply, {error, stopping}, State};
	true ->
	    make_call(Procedure, Params, Timeout, From, State)
    end;
handle_call({call_bulk,Procedure,Params, BulkData, Timeout}, From, State) ->
    if
	State#state.pendinglen >= State#state.pendingmax ->
	    {reply, {error, queue_full}, State};
	State#state.stopping == true ->
	    {reply, {error, stopping}, State};
	true ->
	    make_call_bulk(Procedure, Params, BulkData, Timeout, From, State)
    end;
handle_call(close, From, State) ->
    case State#state.pendinglen of
	0 -> {stop, normal, ok, State};
	_ -> {reply, ok, State#state{ stopping = true }}
    end;
handle_call({set_xid, Xid}, _From, State) ->
    {reply, ok, State#state{ xid = Xid }};
handle_call(get_xid, _From, State) ->
    {reply, State#state.xid, State};
handle_call({set_timeout, Time}, _From, State) ->
    {reply, ok, State#state{ timeout = Time * 1000 }, Time};
handle_call(get_timeout, _From, State) ->
    {reply, State#state.timeout div 1000, State};
handle_call(get_queue_limit, _From, State) ->
    {reply, State#state.pendingmax, State};
handle_call({set_queue_limit, N}, _From, State) ->
    Plen = State#state.pendinglen,
    %% FIXME: is this correct?  shouldn't it be possible to decrease the
    %% the length even if it's currently too large??
    if
	Plen >= N ->
	    {reply, {error, queue_too_long}, State};
	true ->
	    {reply, ok, State#state{ pendingmax = N }}
    end;
handle_call({set_retry_timeout, Time}, _From, State) ->
    {reply, ok, State#state{ retry_timeout = Time * 1000 }};
handle_call(get_retry_timeout, _From, State) ->
    {reply, State#state.retry_timeout div 1000, State};
handle_call(get_vers, _From, State) ->
    {reply, State#state.version, State};
handle_call(get_socket, _From, State) ->
    {reply, State#state.socket, State};
handle_call({set_auth, Auth}, _From, State) ->
    None = {'AUTH_NONE', <<>>},
    S1 = case Auth of
	     auth_null ->
		 State#state{ auth = {None, None} };
	     {auth_sys, Stamp, Name, Uid, Gid, Gids} ->
		 Cred = {'AUTH_SYS',
			 rpc_xdr:enc_authsys_params({Stamp,Name,Uid,Gid,Gids})},
		 State#state{auth = {Cred, None}}
	 end,
    {reply, ok, call_const(S1)};
handle_call(get_auth, _From, State) ->
    {reply, State#state.auth, State};
handle_call(get_queue_length, _From, State) ->
    {reply, State#state.pendinglen, State};
handle_call(get_stats, _From, State) ->
    {reply, {ok, prettyify_stats(State, now())}, State};
handle_call(get_and_reset_stats, _From, State) ->
    Now = now(),
    {reply, compute_stats(State, Now),
     State#state{ statistics = #statistics{ start_time = Now } }};
handle_call({set_owner, NewOwner}, From, State) ->
    %% TODO: we don't store the owner as part of the state, so we
    %% assume that the caller is the current owner, for unlinking
    %% purposes.
    {Owner, _Tag} = From,
    link(NewOwner),
    unlink(Owner),
    {reply, ok, State};
handle_call(Req, _From, State) ->
    {reply, {error, bad_attribute}, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    reply(Packet, State);
handle_info({tcp, _Socket, Packet}, State) ->
    %% Over TCP, reply records may be diveded into fragments.
    %% First, get the Record Marking 32-bit header
    <<Last:1/integer, _Len:31/integer, Block/binary>> = Packet,
    Blocks = State#state.tcp_blocks,
    if Last == 1 ->
	    Bin = if length(Blocks) == 0 -> Block;
		     true -> list_to_binary([Blocks,Block])
		  end,
	    S1 = State#state{tcp_blocks = []},
	    reply(Bin, S1);
       Last == 0 ->
	    {noreply, State#state{tcp_blocks = [Blocks, Block]}}
    end;

handle_info({tcp_closed, Socket}, State) ->
    {stop, shutdown, State};
handle_info({udp_closed, Socket}, State) ->
    {stop, shutdown, State};

handle_info({retry, Xid}, State) ->
    do_retry(Xid, State);

handle_info({timeout, Xid}, State) ->
    do_timeout(Xid, State);

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% make_call: Does the dirty work of encoding an RPC call.
%%% Assumes that the server state has been checked to allow the call.
%%% Returns like handle_call/3 above.

%%% First, generate the XDR-encoded RPC call headers.

make_call(Procedure, Params, Timeout, From, S) ->
    {HeadBin, AuthBin, Sz} = S#state.call_const,
    Call = [<<(S#state.xid):32/integer>>, HeadBin, <<Procedure:32/integer>>,
	    AuthBin, Params],
    ParamsSz = io_list_len(Params),
    make_call1(From, Sz + ParamsSz, Call, Timeout, S, Procedure).

make_call_bulk(Procedure, Params, BulkData, Timeout, From, S) ->
    {HeadBin, AuthBin, Sz} = S#state.call_const,
    Tail = Params, BulkData, enc_align(size(BulkData)),
    TailSz = io_list_len(Tail),
    Call = [<<(S#state.xid):32/integer>>, HeadBin, <<Procedure:32/integer>>,
	    AuthBin, Tail],
    make_call1(From, Sz + TailSz, Call, Timeout, S, Procedure).

%%% Perform transport-dependent work and send the RPC packet.
make_call1(From, Size, Call, Timeout, S, Procedure) when S#state.proto == tcp ->
    %% TODO: ensure Size < 2^31
    Packet = [<<1:1, Size:31>>, Call], % one record marking fragment
    case gen_tcp:send(S#state.socket, Packet) of
	ok ->
	    make_call2(From, Size, [], Timeout, S, Procedure, []);
	{error, Reason} ->
	    error_logger:format("Error: make_call/tcp: ~w\n", [Reason]),
	    {stop, Reason, S}
    end;

make_call1(From, Size, Call, Timeout, S, Procedure) when S#state.proto == udp ->
    io:format("~p\n", [list_to_binary([Call])]),
    case gen_udp:send(S#state.socket, Call) of
	ok ->
	    Timer = erlang:send_after(S#state.retry_timeout, self(),
				      {retry, S#state.xid}),
	    make_call2(From, Size, Call, Timeout, S, Procedure, [Timer]);
	{error, Reason} ->
	    error_logger:format("Error: make_call/udp: ~w\n", [Reason]),
	    {stop, Reason, S}
    end.

%%% Upon a successful send of the packet, update the pending set and stats.

make_call2(From, Size, Call, Timeout, S, Procedure, Timers0) ->
    Ps = S#state.pending,
    Plen = S#state.pendinglen,
    Tm = case Timeout of
	     undefined -> S#state.timeout;
	     _ -> Timeout
	 end,
    Timers1 =
	case Tm of
	    infinity ->
		Timers0;
	    0 ->
		Timers0;
	    _ ->
		Timer = erlang:send_after(Tm, self(), {timeout, S#state.xid}),
		[Timer | Timers0]
	end,
    case Tm of
	0 ->
	    {reply, {error, timeout}, S#state{xid = S#state.xid+1}};
	_ ->
	    Pnew = #pending{from = From, start_time = now(),
			    timers = Timers1,
			    bytes_out = Size,
			    xid = S#state.xid, packet = Call},
	    S1 = S#state{xid = S#state.xid+1, pending = [Pnew|Ps],
			 pendinglen = Plen + 1},
	    {noreply, incr_call_stats(S1, Procedure)}
    end.

%%% Process an RPC reply by extracting caller from pending queue, decoding
%%% the reply RPC header, and updating the pending queue and statistics.

reply(Record, State) ->
    <<Xid:32/integer, _/binary>> = Record,
    Pending = State#state.pending,
    case keysearchdel(Xid, #pending.xid, Pending) of
	{P, Rest} ->
	    lists:foreach({erlang, cancel_timer}, P#pending.timers),
	    Reply = make_reply(Record, State),
	    gen_server:reply(P#pending.from, Reply),
	    Stats = State#state.statistics,
	    Stats1 = update_stats(Stats, P#pending{bytes_in = size(Record)}),
	    Plen = State#state.pendinglen - 1,
	    NewState = State#state{pending = Rest, statistics = Stats1,
				   pendinglen = Plen},
	    if
		Plen == 0, State#state.stopping == true ->
		    {stop, normal, NewState};
		true ->
		    {noreply, incr_invalid_stats_maybe(NewState, Reply)}
	    end;
	false ->
	    %% Request has timed out, silently ignore reply.
	    {noreply, State}
    end.

%%% Decode reply RPC header and translate errors to Erlang descriptions.
%%% May crash upon mismatch (caught).

make_reply(Bin, State) ->
    case catch rpc_xdr:dec_rpc_msg(Bin, 0) of
	{{Xid, {'REPLY', Body}}, ParOff} ->
	    case Body of
		{'MSG_ACCEPTED', {Verf, Stat}} ->
		    case Stat of
			{'SUCCESS', _Null} ->
			    <<_:ParOff/binary,Params/binary>> = Bin,
			    {ok, Params};
			{'PROG_UNAVAIL', _Void} ->
			    {error, 'PROG_UNAVIL'};
			{'PROG_MISMATCH', Lo, Hi} ->
			    {error, {'PROG_MISMATCH', Lo, Hi}};
			{'PROC_UNAVAIL', _Void} ->
			    {error, 'PROC_UNAVAIL'};
			{'GARBAGE_ARGS', _Void} ->
			    {error, 'GARBAGE_ARGS'};
			{'SYSTEM_ERR', _Void} ->
			    {error, 'SYSTEM_ERR'}
		    end;
		{'MSG_DENIED', Rej} ->
		    case Rej of
			{'RPC_MISMATCH', Lo, Hi} ->
			    {error, {'RPC_MISMATCH', Lo, Hi}};
			{'AUTH_ERROR', AuthErr} ->
			    {error, AuthErr}
		    end
	    end;
	{'EXIT', Reason} ->
	    error_logger:format("rpc_client, dec_rpc_msg: ~p\n", [Reason]),
	    {error, cant_decode_res}
    end.

%%% Runtime statistics and performance monitoring.

%%% Update the statistics based on a successful RPC.
update_stats(S, P) when record(S, statistics), record(P, pending) ->
    Dt = now_diff(now(), P#pending.start_time),
    Dt2 = Dt * Dt,
    S#statistics{
	replies = S#statistics.replies + 1,
        bytes_in = S#statistics.bytes_in + P#pending.bytes_in,
        bytes_out = S#statistics.bytes_out + P#pending.bytes_out,
        lat_sumt = S#statistics.lat_sumt + Dt,
        lat_sumt2 = S#statistics.lat_sumt2 + Dt2
    }.

%%% Translate the accumulated statistics to a list of reports.
compute_stats(State, Now) when record(State, state) ->
    S = State#state.statistics,
    DeltaT = now_diff(Now, S#statistics.start_time),
    N = S#statistics.replies,
    {MeanLatency, DevLatency} = case N of
        0 -> {0, 0};				% avoid division by zero
        1 -> {S#statistics.lat_sumt, 0};	% avoid computing zero std. dev.
        _ ->
            U = S#statistics.lat_sumt / N,
            Var = S#statistics.lat_sumt2 / N - U * U,
            Dev = math:sqrt(Var),
            {U, Dev}
    end,
    [
        {elapsed_time, DeltaT},
        {pending_requests, State#state.pendinglen},
        {request_limit, State#state.pendingmax},
        {bytes_in, S#statistics.bytes_in},
        {bytes_out, S#statistics.bytes_out},
        {mean_latency, MeanLatency},
        {dev_latency, DevLatency}
    ].

prettyify_stats(State, Now) ->
    Stats = State#state.statistics,
    lists:append([[
		   {ip, State#state.ip},
		   {port, State#state.port},
		   {proto, State#state.proto},
		   {rpc_program, State#state.program},
		   {rpc_version, State#state.version}
		  ],
		  compute_stats(State, now()),
		  [{per_op_counts, Stats#statistics.proccounts},
		   {calls, Stats#statistics.calls},
		   {replies, Stats#statistics.replies},
		   {timedout, Stats#statistics.timedout},
		   {invalid, Stats#statistics.invalid},
		   {retries, Stats#statistics.retries}
		  ]]).

%%% Return the difference between two times returned from
%%% erlang:now(), in integer microseconds.
now_diff({MegaSec1, Sec1, MicroSec1}, {MegaSec2, Sec2, MicroSec2}) ->
    ((MegaSec1 - MegaSec2) * 1000000
     + (Sec1 - Sec2)) * 1000000
    + (MicroSec1 - MicroSec2).


do_retry(Xid, State) ->
    case keysearchdel(Xid, #pending.xid, State#state.pending) of
	{Pend, Rest} ->
	    case gen_udp:send(State#state.socket, Pend#pending.packet) of
		ok ->
		    {noreply, incr_retries_stats(State)};
		{error, Reason} ->
		    {stop, Reason, State}
	    end;
	false ->
	    {noreply, State}
    end.
	    
do_timeout(Xid, State) ->
    case keysearchdel(Xid, #pending.xid, State#state.pending) of
	{Pend, Rest} ->
	    gen_server:reply(Pend#pending.from, {error, timeout}),
	    S1 = State#state{pending = Rest,
			     pendinglen = State#state.pendinglen - 1},
	    {noreply, incr_timeout_stats(S1)};
	false ->
	    {noreply, State}
    end.


incr_call_stats(S, ProcNum) ->
    ProcIdx = ProcNum + 1,
    Stats = S#state.statistics,
    Reqs = Stats#statistics.calls,
    OldProcCountsLen = Stats#statistics.proccounts_len,
    OldProcCounts = if
			ProcIdx > OldProcCountsLen ->
			    L = tuple_to_list(Stats#statistics.proccounts),
			    extend_proccounts_list(L, ProcIdx);
			true ->
			    Stats#statistics.proccounts
		    end,
    NewProcCounts = setelement(ProcIdx, OldProcCounts,
			       element(ProcIdx, OldProcCounts) + 1),
    S#state{statistics =
	    Stats#statistics{proccounts = NewProcCounts,
			     proccounts_len = size(NewProcCounts),
			     calls = Reqs + 1}}.

extend_proccounts_list(L, MaxLen) ->
    list_to_tuple(lists:append(L, lists:duplicate(MaxLen - length(L), 0))).

incr_timeout_stats(S) ->
    Stats = S#state.statistics,
    Timedout = Stats#statistics.timedout,
    S#state{statistics = Stats#statistics{ timedout = Timedout + 1 }}.

incr_invalid_stats_maybe(S, Reply) ->
    case Reply of
	{ok, _} ->
	    S;
	_ ->
	    Stats = S#state.statistics,
	    Invalid = Stats#statistics.invalid,
	    S#state{statistics =
		    Stats#statistics{ invalid = Invalid + 1 }}
    end.

incr_retries_stats(S) ->
    Stats = S#state.statistics,
    Retries = Stats#statistics.retries,
    S#state{statistics = Stats#statistics{ retries = Retries + 1 }}.

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

enc_align(Len) ->
    case Len rem 4 of
	0 -> <<>>;
	1 -> <<0,0,0>>;
	2 -> <<0,0>>;
	3 -> <<0>>
   end.

keysearchdel(Key, N, L) ->
    keysearchdel(L, Key, N, []).

keysearchdel([H|T], Key, N, Acc) when element(N, H) == Key ->
    {H, Acc ++ T};
keysearchdel([H|T], Key, N, Acc) ->
    keysearchdel(T, Key, N, [H|Acc]);
keysearchdel([], Key, N, Acc) ->
    false.

call_const(S) ->
    {Cred, Verf} = S#state.auth,
    HeadBin = list_to_binary(
		[rpc_xdr:enc_msg_type('CALL'),
		 <<?RPC_VERSION_2:32/integer,
		 (S#state.program):32/integer,
		 (S#state.version):32/integer>>]),
    AuthBin = list_to_binary([rpc_xdr:enc_opaque_auth(Cred),
			      rpc_xdr:enc_opaque_auth(Verf)]),
    XidSz = 4,
    ProcSz = 4,
    Sz = XidSz + size(HeadBin) + ProcSz + size(AuthBin),
    S#state{call_const = {HeadBin, AuthBin, Sz}}.
