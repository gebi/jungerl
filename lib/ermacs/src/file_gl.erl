%%%----------------------------------------------------------------------
%%% File    : file_gl.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Group leader server for writing to a file
%%% Created : 22 Oct 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(file_gl).
-author('luke@bluetail.com').

-behaviour(gen_server).

%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3,
	 handle_call/3, handle_cast/2]).

-record(state, {fd}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Filename) ->
    gen_server:start_link({local, file_gl}, file_gl, Filename, []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init(Filename) ->
    {ok, Fd} = file:open(Filename, [write]),
    {ok, #state{fd=Fd}}.

handle_info({io_request, From, ReplyAs, {put_chars, C}}, State) ->
    file:write(State#state.fd, C),
    From ! {io_reply, ReplyAs, ok},
    {noreply, State};
handle_info({io_request, From, ReplyAs, {put_chars, M, F, A}}, State) ->
    file:write(State#state.fd, apply(M, F, A)),
    From ! {io_reply, ReplyAs, ok},
    {noreply, State};
handle_info({io_request, From, ReplyAs, {get_until, _, _, _}}, State) ->
    From ! {io_reply, ReplyAs, eof},
    {noreply, State};
handle_info(Info, State) ->
    {noreply, State}.

handle_call(_, _, _) ->
    exit(no_such_callback).

handle_cast(_, _) ->
    exit(no_such_callback).

terminate(Reason, State) ->
    file:close(State#state.fd),
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

