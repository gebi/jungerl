%%%-------------------------------------------------------------------
%%% File    : proc_tabs.erl
%%% Author  : Ulf Wiger <etxuwig@ws12858>
%%% Description : 
%%%
%%% Created :  6 Mar 2006 by Ulf Wiger <etxuwig@ws12858>
%%%-------------------------------------------------------------------
-module(proc_tabs).
-behaviour(gen_server).

-export([ets_new/2]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


ets_new(Name, Opts) ->
    gen_server:call(?MODULE, {ets_new, Name, Opts}).


start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).


init(_) ->
    {ok, []}.

handle_call({ets_new, Name, Opts}, _From, S) ->
    Res = ets:new(Name, Opts),
    {reply, Res, S}.

handle_cast(_Msg, S) ->
    {stop, unknown_cast, S}.

handle_info(_Msg, S) ->
    {noreply, S}.


terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.
