%%%----------------------------------------------------------------------
%%% File    : edit_var.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Variable management server - transient and persistent
%%% Created : 21 Jan 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

%%% This module implements "setq"-like variables. But, this seems a bit
%%% distasteful because of concurrent updates and so on. Maybe there is
%%% better way to do variables in general (or just program-internal
%%% variables).

-module(edit_var).
-author('luke@bluetail.com').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

-export([lookup/1, lookup/2, set/2, permanent/2, add_to_list/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {ets, dets}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, edit_var}, edit_var, [], []).

%% Return: Value | undefined
lookup(Name) ->
    lookup(Name, undefined).

lookup(Name, Default) ->
    gen_server:call(?MODULE, {lookup, Name, Default}).

set(Name, Value) ->
    gen_server:call(?MODULE, {set, Name, Value}).

%% permanent(Name, true | false)
permanent(Name, Flag) ->
    gen_server:call(?MODULE, {permanent, Name, Flag}).

add_to_list(Name, Value) ->
    List = lookup(Name, []),
    edit_var:set(Name, include(List, Value)).

include([], Value)        -> [Value];
include([Value|T], Value) -> [Value|T];
include([H|T], Value)     -> [H|include(T, Value)].

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    Filename = filename:join(os:getenv("HOME"), "edit_var.dets"),
    Ets = ets:new(edit_mem_var, [set, public, named_table]),
    {ok, Dets} = dets:open_file(edit_disk_var,
				[{type, set},
				 {file, Filename}]),
    load_file(Dets, Ets),
    State = #state{ets=Ets,
		   dets=Dets},
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({lookup, Name, Default}, From, State) ->
    {reply, do_lookup(State, Name, Default), State};

handle_call({set, Name, Value}, From, State) ->
    ets:insert(State#state.ets, {Name, Value}),
    case is_permanent(State, Name) of
	true ->
	    dets:insert(State#state.dets, {Name, Value});
	false ->
	    ok
    end,
    {reply, ok, State};

handle_call({permanent, Name, Flag}, From, State) ->
    case Flag of
	false ->
	    dets:delete(State#state.dets, Name);
	true ->
	    dets:insert(State#state.dets, {Name, do_lookup(State, Name)})
    end,
    {reply, ok, State}.

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

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

load_file(Dets, Ets) ->
    F = fun(X) -> ets:insert(Ets, X) end,
    dets:traverse(Dets, F).

do_lookup(State, Name) ->
    do_lookup(State, Name, undefined).

do_lookup(State, Name, Default) ->
    case ets:lookup(State#state.ets, Name) of
	[{_, Value}] ->
	    Value;
	[] ->
	    Default
    end.

is_permanent(State, Name) ->
    case dets:lookup(State#state.dets, Name) of
	[] ->
	    false;
	_ ->
	    true
    end.
