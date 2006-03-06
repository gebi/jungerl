%%%-------------------------------------------------------------------
%%% File    : proc_super.erl
%%% Author  : Ulf Wiger <etxuwig@ws12858>
%%% Description : 
%%%
%%% Created :  6 Mar 2006 by Ulf Wiger <etxuwig@ws12858>
%%%-------------------------------------------------------------------
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
