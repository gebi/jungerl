%%%-------------------------------------------------------------------
%%% File    : proc_reg_tabs.erl
%%% Author  : Ulf Wiger <etxuwig@wsb221>
%%% Description : 
%%%
%%% Created :  5 Jul 2004 by Ulf Wiger <etxuwig@wsb221>
%%%-------------------------------------------------------------------
-module(proc_reg_tabs).
-behaviour(gen_server).

-export([start_link/0]).
-export([attach/0]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


start_link() ->
    gen_server:start_link({local, proc_reg_tabs}, ?MODULE, [], []).


attach() ->
    gen_server:call(proc_reg_tabs, {attach, self()}).


init([]) ->
    process_flag(priority, high),
    ets:new(proc_reg, [set, public, named_table]),
    ets:new(proc_reg_rev, [ordered_set, public, named_table]),
    io:format("tabs created.~n", []),
    {ok, undefined}.


handle_call({attach, Pid}, _From, undefined) when pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    {reply, ok, MRef}.

handle_cast(Msg, _S) ->
    {stop, {unknown_cast,Msg}}.

handle_info({'DOWN', MRef, process, _, _}, MRef) ->
    {noreply, undefined};
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.
