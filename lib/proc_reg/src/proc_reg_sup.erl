%%%-------------------------------------------------------------------
%%% File    : proc_reg_sup.erl
%%% Author  : Ulf Wiger <etxuwig@wsb221>
%%% Description : 
%%%
%%% Created :  5 Jul 2004 by Ulf Wiger <etxuwig@wsb221>
%%%-------------------------------------------------------------------
-module(proc_reg_sup).

-behaviour(supervisor).

-export([start/2, stop/1]).
-export([start_link/0,
	 init/1]).


start(_Type, _Args) ->
    start_link().

stop(_) ->
    ok.



start_link() ->
    supervisor:start_link({local, proc_reg_sup}, ?MODULE, top).



init(top) ->
    io:format("init(top)~n", []),
    {ok, {{one_for_all, 0, 1},
	  [{tab_sup, {supervisor,start_link,[{local,proc_reg_tab_sup}, ?MODULE, tab]},
	    permanent, infinity, supervisor, [?MODULE]},
	   {reg_sup, {supervisor,start_link,[{local,proc_reg_reg_sup}, ?MODULE, reg]},
	    permanent, infinity, supervisor, [?MODULE]}]}};
init(tab) ->
    io:format("init(tab)~n", []),
    {ok, {{one_for_all, 0, 1},
	  [{tab_owner, {proc_reg_tabs, start_link, []},
	    permanent, 1000, worker, [proc_reg_tabs]}]}};
init(reg) ->
    io:format("init(reg)~n", []),
    {ok, {{one_for_one, 3, 10},
	  [{proc_reg, {proc_reg, start_link, []},
	    permanent, 3000, worker, [proc_reg]}]}}.

