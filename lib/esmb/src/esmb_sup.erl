%%%----------------------------------------------------------------------
%%% File    : esmb_sup.erl
%%% Author  : tobbe@bluetail.com
%%% Purpose : Supervisor for the esmb stuff.
%%% Created : 6 Apr 20034
%%%
%%% $Id$
%%%----------------------------------------------------------------------
-module(esmb_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, esmb_sup}, esmb_sup, []).

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
    MD4driver = {md4,{md4,start_link,[]},
		 permanent,5000,worker,[md4]},
    Iconvdriver = {iconv,{iconv,start_link,[]},
		   permanent,5000,worker,[iconv]},
    {ok,{{one_for_one,3,10}, [MD4driver, Iconvdriver]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
