%%%-------------------------------------------------------------------
%%% File    : qc_example.erl
%%% Purpose : Examples using QuickCheck.
%%%
%%% Created : 19 Nov 2003 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(qc_example).

-include_lib("quickcheck/include/quickcheck.hrl").

-compile(export_all).

%% Test OTP 'sets' module.
%% This test is copied from John Hughes's EUC slides:
%%   http://www.math.chalmers.se/~rjmh/ErlangQC/

%% Entry function.
check_sets() ->
    qc:quickcheck(prop_union_commutes()).

equal(S1, S2) ->
    lists:sort(sets:to_list(S1)) == lists:sort(sets:to_list(S2)).

%% Test that set union is commutative.
prop_union_commutes() ->
    ?FORALL(X, set(),
	    ?FORALL(Y, set(),
		    equal(sets:union(X, Y), sets:union(Y, X)))).

%% Set generator.

%% Simple working version.
set() ->
    qc:frequency(
      [{6, ?LET(L, qc:list(qc:int()),
		qc:return({'@', sets, from_list, [L]}))}]).

%% This one from the paper doesn't work with the code from the
%% web. The recursive set() use in the second generator causes
%% infinite recursion.
set1() ->
    qc:frequency(
      [{6, ?LET(L, qc:list(qc:int()),
		qc:return({'@', sets, from_list, [L]}))},
       {6, ?LET(S, set1(),
		?LET(E, qc:int(),
		     qc:return({'@', sets, add_element, [E, S]})))},
       {1, ?LET(P, qc:function(qc:bool()),
		?LET(S, set1(),
		     qc:return({'@', sets, filter, [P, S]})))}]).

