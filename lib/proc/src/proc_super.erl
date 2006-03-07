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
%%% File:	proc_super.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Supervisor callback module for the proc application
%%% 
%%%----------------------------------------------------------------------
-module(proc_super).
-behaviour(supervisor).


-export([start_link/1,
	 init/1]).


start_link(Type) when Type == top; Type == sub ->
    supervisor:start_link({local, name_of(Type)}, ?MODULE, Type).


name_of(sub) -> proc_sup_sub;
name_of(top) -> proc_super.
    

init(top) ->
    RestartStrategy = one_for_all,
    MaxR = 0, MaxT = 1,
    ChildSpecs =
	[{proc_tabs, {proc_tabs, start_link, []},
	  permanent, 3000, worker, [proc_tabs]},
	 {proc_sup_sub, {proc_super, start_link, [sub]},
	  permanent, infinity, supervisor, [?MODULE]}],
    {ok, {{RestartStrategy, MaxR, MaxT}, ChildSpecs}};
init(sub) ->
    RestartStrategy = one_for_one,
    MaxR = 3,
    MaxT = 10,
    ChildSpecs =
	[{proc, {proc, start_link, []}, 
	  permanent, 10000, worker, [proc]}],
    {ok, {{RestartStrategy, MaxR, MaxT}, ChildSpecs}}.
