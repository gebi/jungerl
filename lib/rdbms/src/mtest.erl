%%%-------------------------------------------------------------------
%%% File    : mtest.erl
%%% Author  : Ulf Wiger <etxuwig@ws12858>
%%% Description : 
%%%
%%% Created : 27 Feb 2006 by Ulf Wiger <etxuwig@ws12858>
%%%-------------------------------------------------------------------
-module(mtest).

-export([f/4,
	 m/2]).


f(Objs,M,F,Pattern) ->
    lists:filter(
      fun(Obj) ->
	      true == M:F(Obj)
      end, Objs).


m(Objs,Pat) ->
    ets:match_spec_run(Objs, ets:match_spec_compile(Pat)).
