%%%-------------------------------------------------------------------
%%% File    : filtertest.erl
%%% Author  : Ulf Wiger <etxuwig@ws12858>
%%% Description : 
%%%
%%% Created : 27 Feb 2006 by Ulf Wiger <etxuwig@ws12858>
%%%-------------------------------------------------------------------
-module(filtertest).

-export([f/2]).



f(Obj, Pat) ->
    ets:match_spec_run([Obj], ets:match_spec_compile(Pat)).
