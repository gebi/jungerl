%%%----------------------------------------------------------------------
%%% File    : test3_service.erl
%%% Purpose : Old-style-rpc_gen method for implementing server-side.
%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%%%----------------------------------------------------------------------

-module(test_prog1_2).

-behaviour(gen_server).

-define(NAME, ?MODULE).
-define(Timeout, infinity).

%% External exports
-export([start_link/0]).
-export([fnull/0, fnull/1, double/1, double/2, repeat/1, repeat/2,
	 pow/1, pow/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

%% These are not strictly necessary for use with the _svc.erl stubs, but
%% they're helpful for debugging.

fnull() ->
    fnull(?Timeout).
fnull(Timeout) ->
    gen_server:call(?NAME, {fnull_2}, Timeout).

double(N) ->
    double(N, ?Timeout).
double(N, Timeout) ->
    gen_server:call(?NAME, {double_2, N}, Timeout).

repeat({N, S}) ->
    repeat({N, S}, ?Timeout).
repeat({N, S}, Timeout) ->
    gen_server:call(?NAME, {repeat_2, {N, S}}, Timeout).

pow({N, M}) ->
    pow({N, M}, ?Timeout).
pow({N, M}, Timeout) ->
    gen_server:call(?NAME, {pow2, {N, M}}, Timeout).

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
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({fnull_2}, From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({double_2, N}, From, State) ->
    Reply = N * 2,
    {reply, Reply, State};
handle_call({repeat_2, {N, S}}, From, State) ->
    Reply = lists:flatten(lists:duplicate(N, S)),
    {reply, Reply, State};
handle_call({pow_2, {N, M}}, From, State) ->
    Reply = 
	case catch math:pow(N, M) of
	    Pow when float(Pow) ->
		{'STATUS_OK', {"Hello, world!\n", Pow}};
	    _Error ->
		{'STATUS_ERR', void}
	end,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("XXXYYYXXX ~w: ~s:handle_cast got ~w\n", [self(), ?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("XXXYYYXXX ~w: ~s:handle_info got ~w\n", [self(), ?MODULE, Info]),
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

