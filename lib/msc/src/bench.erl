%%%-------------------------------------------------------------------
%%% File    : bench.erl
%%% Author  : Luke Gorrie <luke@synap.se>
%%% Purpose : Benchmarking utilities.
%%%
%%% Created :  7 Oct 2004 by Luke Gorrie <luke@synap.se>
%%%-------------------------------------------------------------------
-module(bench).

-export([dotimes/2]).

%% Call Fun N times and print the total/min/avg/max/mdev runtime.
dotimes(N, Fun) when integer(N), N > 0 ->
    print_stats(runtimes(N, Fun)).

%% Print simple statistics about Values, a list of integers.
%% The optional second argument is the name of the units, default 'ms'.
print_stats(Values) -> print_stats(Values, ms).

print_stats(Values, Units) ->
    io:format("total = ~.3f ~s, min/avg/max/mdev = ~.3f/~.3f/~.3f/~.3f ~s~n",
	      [sum(Values)/1000, Units, min(Values)/1000, mean(Values)/1000,
	       max(Values)/1000, mdev(Values)/1000, Units]).

max(Values) -> lists:max(Values).
min(Values) -> lists:min(Values).
sum(Values) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, Values).
mean(Values) -> sum(Values) / length(Values).
mdev(Values) ->
    M = mean(Values),
    sum([abs(V - M) || V <- Values]).

%% Call Fun N times and return the list of running times.
runtimes(N, Fun) -> runtimes(N, Fun, []).

runtimes(0, F, Times) ->
    Times;
runtimes(N, F, Times) when N > 0 ->
    case timer:tc(erlang,apply,[F,[]]) of
	{_, {'EXIT',Reason}} ->
	    exit(Reason);
	{Time, _} ->
	    runtimes(N-1, F, [Time|Times])
    end.

