%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is proc-1.0.
%%%
%%% The Initial Developer of the Original Code is Ericsson AB
%%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB
%%% All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:	proc_tabs.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Table-owner process for proc.
%%% 
%%%----------------------------------------------------------------------
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
