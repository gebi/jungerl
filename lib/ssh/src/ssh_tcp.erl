%%%-------------------------------------------------------------------
%%% File    : ssh_tcp.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : SSH port forwarding
%%%
%%% Created : 24 Aug 2004 by Tony Rogvall <tony@bix.hemma.se>
%%%-------------------------------------------------------------------
-module(ssh_tcp).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start/1, start_link/1]).
-export([start/3, start_link/3]).

-export([forward/5, backward/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
	{
	  cm   %% connection manager
	 }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link(CM) ->
    gen_server:start_link(?MODULE, [CM], []).
    
start_link(Host, Port, Auth) ->
    gen_server:start_link(?MODULE, [Host,Port,Auth], []).

start(CM) ->
    gen_server:start(?MODULE, [CM], []).
    
start(Host, Port, Auth) ->
    gen_server:start(?MODULE, [Host,Port,Auth], []).


forward(Pid, LocalIP, LocalPort, RemoteIP, RemotePort) ->
    gen_server:call(Pid, {forward, 
			  LocalIP, LocalPort,
			  RemoteIP, RemotePort}).

backward(Pid, LocalIP, LocalPort, RemoteIP, RemotePort) ->
    gen_server:call(Pid, {backward,
			  LocalIP, LocalPort,
			  RemoteIP, RemotePort}).
    
%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([CM]) ->
    case ssh_cm:attach(CM) of
	{ok,CMPid} ->
	    {ok, #state { cm = CMPid }};
	Error ->
	    {stop, Error }
    end;
init([Host,Port,Auth]) ->
    case ssh_cm:start_link(undefined, Host, Port, Auth) of
	{ok, CM} ->
	    {ok, #state { cm = CM }};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call({forward, LocalIP, LocalPort, RemoteIP, RemotePort},From,State) ->
    LIP = ip_address(LocalIP),
    RIP = ip_address(RemoteIP),
    Me = self(),
    Prog = fun(S) ->
		   io:format("accepted\n"),
		   gen_tcp:controlling_process(S, Me),
		   gen_server:cast(Me,{forward, S, RemoteIP, RemotePort})
	   end,
    case ssh_tcp_wrap:spawn_server(LocalPort,
				   [{ifaddr, LIP},{mode, binary},
				    {packet, 0},{active, false}],Prog) of
	{ok,Server,ListenPort} ->
	    {reply, {ok,ListenPort}, State};
	Error ->
	    {reply, Error, State}
    end;
handle_call({backward, LocalIP, LocalPort, RemoteIP, RemotePort},From,State) ->
    case ssh_cm:tcpip_forward(State#state.cm, RemoteIP, RemotePort) of
	ok ->
	    put({ipmap,{RemoteIP,RemotePort}}, {LocalIP,LocalPort}),
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call(Request, From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({forward, S, RemoteIP, RemotePort}, State) ->
    case inet:peername(S) of
	{ok,{OrigIP, OrigPort}} ->
	    io:format("peer ~p ~p remote ~p ~p\n", 
		      [OrigIP, OrigPort, RemoteIP, RemotePort]),
	    case ssh_cm:direct_tcpip(State#state.cm, 
				     RemoteIP, RemotePort,
				     OrigIP, OrigPort) of
		{ok, Channel} ->
		    io:format("got channel ~p\n", [Channel]),
		    ssh_cm:set_user_ack(State#state.cm, Channel, true),
		    put({channel,S}, Channel),
		    put({socket,Channel}, S),
		    inet:setopts(S, [{active, once}]),
		    {noreply, State};
		{error, Error} ->
		    io:format("forward: error ~p\n", [Error]),
		    gen_tcp:close(S),
		    {noreply, State}
	    end;
	Error ->
	    gen_tcp:close(S),
	    {noreply, State}
    end;
    
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_info({tcp, S, Data}, State) ->
    case get({channel,S}) of
	undefined ->
	    {noreply, State};	    
	Channel ->
	    io:format("sending ~p -> ~p\n", [S, Channel]),
	    %% send and wait for ack
	    ssh_cm:send_ack(State#state.cm, Channel, Data),
	    inet:setopts(S, [{active, once}]),
	    {noreply, State}
    end;
handle_info({tcp_closed, S}, State) ->
    io:format("tcp: closed: ~p\n", [S]),
    case get({channel,S}) of
	undefined -> 
	    {noreply, State};
	Channel ->
	    ssh_cm:send_eof(State#state.cm, Channel),
	    {noreply, State}
    end;

handle_info({ssh_cm, CM, {data, Channel, Type, Data}}, State) ->
    if Type == 0 ->
	    io:format("ssh_cm: data: ~p\n", [Channel]),
	    case get({socket,Channel}) of
		undefined ->{noreply, State};
		S ->
		    io:format("sending ~p -> ~p\n", [Channel,S]),
		    gen_tcp:send(S, Data),
		    ssh_cm:adjust_window(CM, Channel, size(Data)),
		    {noreply, State}
	    end;
       true  ->
	    io:format("STDERR: ~s\n", [binary_to_list(Data)]),
	    ssh_cm:adjust_window(CM, Channel, size(Data)),
	    {noreply, State}
    end;

handle_info({ssh_cm, CM, {closed, Channel}}, State) ->
    io:format("ssh_cm: closed: ~p\n", [Channel]),
    case get({socket, Channel}) of
	undefined -> {noreply, State};
	S ->
	    erase({socket,Channel}),
	    erase({channel,S}),
	    gen_tcp:close(S),
	    {noreply, State}
    end;

handle_info({ssh_cm, CM, {eof, Channel}}, State) ->
    io:format("ssh_cm: eof: ~p\n", [Channel]),
    case get({socket,Channel}) of
	undefined -> {noreply, State};
	S ->
	    gen_tcp:shutdown(S, write),
	    {noreply, State}
    end;

handle_info({open, Channel, {forwarded_tcpip,
			     RemoteAddr, RemotePort,
			     OrigIp, OrigPort}}, State) ->
    case get({ipmap,{RemoteAddr,RemotePort}}) of
	undefined ->
	    ssh_cm:close(State#state.cm, Channel),
	    {noreply, State};
	{LocalIP, LocalPort} ->
	    case gen_tcp:connect(LocalIP, LocalPort, [{active,once},
						      {mode,binary},
						      {packet,0}]) of
		{ok, S} ->
		    %% We want ack on send!
		    ssh_cm:set_user_ack(State#state.cm, Channel, true),
		    %% FIXME: set fake peer and port?
		    put({channel, S}, Channel),
		    put({socket,Channel}, S),
		    {noreply, State};
		Error ->
		    ssh_cm:close(State#state.cm, Channel),
		    {noreply, State}
	    end
    end;
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions 
%%--------------------------------------------------------------------

%% Try to convert to ip4/ip6 address tuple	    
ip_address(Addr) when tuple(Addr) ->
    Addr;
ip_address(local) -> local;
ip_address(any)   -> any;
ip_address(Addr) when list(Addr) ->
    case inet_parse:address(Addr) of
	{error, _} -> Addr;
	{ok,A} -> A
    end;
ip_address(A) -> A.

    
