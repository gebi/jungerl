%%%----------------------------------------------------------------------
%%% File    : edit_genserv.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Generic utility functions for servers.
%%% Created : 28 Apr 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

%% This is a library for writing servers. The idea is to have a less
%% abstract gen_server, in which the protocols are exposed and the
%% servers still use 'receive' to get requests with pattern matching.

-module(edit_genserv).
-author('luke@bluetail.com').

-export([start_link/4, call/2, call/3, reply/2, cast/2]).

%% Start a named process, if it's not already running.
%%
%% Returns: {ok, Pid} | {error, Reason}
start_link(Name, M, F, A) ->
    case whereis(Name) of
	Pid when pid(Pid) ->
	    {error, {already_started, Pid}};
	undefined ->
	    Pid = spawn_link(M, F, A),
	    register(Name, Pid),
	    {ok, Pid}
    end.

%% Protocol for calls (from 'gen'):
%% -> {call, {SenderPid, Ref}, Req}
%% <- {Ref, Reply}

%% Synchronous call.
call(Pid, Req) ->
    call(Pid, Req, 10000).

call(Pid, Req, Timeout) ->
    {ok, Reply} = gen:call(Pid, call, Req, Timeout),
    Reply.

reply(To, Reply) ->
    gen:reply(To, Reply).

%% Protocol for casts:
%% -> {cast, Req}

cast(Pid, Req) ->
    Pid ! {cast, Req}.

