%%% File    : ucs_sup.erl
%%% Author  : Johan Blom <johan@localhost.localdomain>
%%% Description : ucs supervisor
%%% Created : 15 Apr 2002 by Johan Blom <johan@mobilearts.se>
-module(ucs_sup).
-author('johan.blom@mobilearts.se').
-vsn('1').

-behaviour(supervisor).

%% External exports
-export([start_link/0,start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(_) ->
    supervisor:start_link({local, ucs_sup}, ucs_sup, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([]) ->
    AChild = {ucs_data, {ucs_data,start_link,[]},
	      permanent, 1000, worker, [ucs_data]},
    {ok,{{one_for_all,4,2000}, [AChild]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
