%%%-------------------------------------------------------------------
%%% File    : fsm_example.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : Example illustrating the use of 
%%%
%%% Created : 28 Jan 2004 by Ulf Wiger <ulf.wiger@ericsson.com>
%%%-------------------------------------------------------------------
-module(fsm_example).
-behaviour(plain_fsm).

%%% Here's the plan:
%%% plain_fsm.erl will include behaviour_info(), 
%%% the exported function plain_fsm:extended_receive/1
%%% (which should never actually be called -- it will simply exit),
%%% and the support functions spawn_link(), system_continue(),
%%% system_code_change() et al.
%%%
%%% Users of plain_fsm must abide by a few rules:
%%%
%%% - at least one receive clause somewhere should be wrapped
%%%   inside a plain_fsm:extended_receive(receive ... end). 
%%%   This will ensure that system messages are handled, including
%%%   the shutdown protocol, without giving up selective receive.
%%% - The function containing the extended_receive wrapper should
%%%   have exactly one argument -- the 'State'.
%%%
-export([spawn_link/0]).
-compile(export_all).
-include("plain_fsm.hrl").

data_vsn() ->
    5.

spawn_link() ->
    plain_fsm:spawn_link(?MODULE, fun() ->
					  process_flag(trap_exit,true),
					  idle(mystate)
				  end).



idle(S) ->
    plain_fsm:extended_receive(
      receive
	  a ->
	      io:format("going to state a~n", []),
	      plain_fsm:hibernate(?MODULE,a,[S]);
	  b ->
	      io:format("going to state b~n", []),
	      b(S)
      after 10000 ->
	      io:format("timeout in idle~n", []),
	      idle(S)
      end).


a(S) ->
    receive
	b ->
	    io:format("going to state b~n", []),
	    plain_fsm:hibernate(?MODULE,b,[S]);
	idle ->
	    io:format("going to state idle~n", []),
	    idle(S)
%       after 10000 ->
% 	      io:format("timeout in a~n", []),
% 	      idle(S)
    end.

b(S) ->
    receive
	a ->
	    io:format("going to state a~n", []),
	    a(S);
	idle ->
	    io:format("going to state idle~n", []),
	    idle(S)
    after 10000 ->
	    io:format("timeout in b~n", []),
	    idle(S)
    end.

code_change(OldVsn, State, Extra) ->
    {ok, {newstate, data_vsn()}}.
 
	     
