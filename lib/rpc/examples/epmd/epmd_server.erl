%%%----------------------------------------------------------------------
%%% File    : epmd_server.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : Epmd server
%%% Created : 20 Aug 1997 by Tony Rogvall <tony@erix.ericsson.se>
%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%%%----------------------------------------------------------------------

-module(epmd_server).
-author('tony@erix.ericsson.se').

-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(portmap, 
	{
	 key,       %% (Node,Service,Vers,Prot)
	 node,
	 service,
	 vers,
	 prot,
	 port
	}).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, epmd_server}, epmd_server, [], []).

stop() ->
    gen_server:call(epmd_server, stop).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    ets:new(epmd_reg, [named_table, {keypos,2}]),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({epmdproc_null_1}, From, State) ->
    {reply, void, State};
handle_call({epmdproc_set_1, {Node,Service,Vers,Prot,Port}}, From, State) ->
    ets:insert(empd_reg, #portmap {
				   key = {Node,Service,Vers,Prot},
				   node = Node,
				   service = Service,
				   vers = Vers,
				   prot = Prot,
				   port = Port
				  }),
    {reply, true, State};
handle_call({epmdproc_unset_1, {Node,Service,Vers,Prot,Port}}, From, State) ->
    ets:delete(epmd_reg, {Node,Service,Vers,Prot}),
    {reply, true, State};
handle_call({epmdproc_getport_1, {Node,Service,Vers,Prot,Port}}, From, State) ->
    case ets:lookup(epmd_reg, {Node,Service,Vers,Prot}) of
	[] -> {reply, 0, State};
	[PM] -> {reply, PM#portmap.port, State}
    end;
handle_call({epmdproc_dump_1}, From, State) ->
    MapList = build_map(ets:first(epmd_reg), void),
    {reply, MapList, State};
handle_call(stop, From, State) ->
    {stop, stopped, ok, State};
handle_call(Request, From, State) ->
    {noreply, State}.

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
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

build_map('$end_of_table', Map) -> Map;
build_map(Key, Map) ->
    case ets:lookup(epmd_reg, Key) of
	[PM] when record(PM, portmap) ->
	    build_map(ets:next(epmd_reg,Key),
		      {{PM#portmap.node,
			PM#portmap.service,
			PM#portmap.vers,
			PM#portmap.prot,
			PM#portmap.port}, Map});
	_ ->
	    build_map(ets:next(epmd_reg,Key),Map)
    end.
