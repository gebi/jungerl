%%%----------------------------------------------------------------------
%%% File    : stack.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : Simple stack
%%% Created : 20 Aug 1997 by Tony Rogvall <tony@erix.ericsson.se>
%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%%%----------------------------------------------------------------------

-module(stack).
-author('tony@erix.ericsson.se').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

%% External exports
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, { stack = [] }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->  
    gen_server:start({local, stack}, stack, [], []).

stop() ->
    gen_server:call(stack, stop).

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
    {ok, #state{ stack = []}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({stackproc_null_1}, From, State) ->
    io:format("STACKPROC: NULL~n"),
    {reply, ok, State};
handle_call({stackproc_pop_1}, From, State) ->
    io:format("STACKPROC: POP~n"),
    case State#state.stack of
	[] -> {reply, 0, State};
	[H|T] -> 
	    {reply, H, State#state {stack = T}}
    end;
handle_call({stackproc_push_1,X}, From, State) ->
    io:format("STACKPROC: PUSH ~w~n", [X]),
    Stack = State#state.stack,
    {reply, true, State#state {stack = [X|Stack]}};
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
