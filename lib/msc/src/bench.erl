%%%-------------------------------------------------------------------
%%% File    : bench.erl
%%% Author  : Luke Gorrie <luke@synap.se>
%%% Purpose : Benchmarking utilities.
%%%
%%% Created :  7 Oct 2004 by Luke Gorrie <luke@synap.se>
%%%-------------------------------------------------------------------
-module(bench).

%% High-level interface.
-export([dotimes/2]).

%% Useful utilities.
-export([print_stats/1, print_stats/2, runtimes/2, plot/1]).

-compile(export_all).
-import(lists, [foreach/2, foldl/3]).

%% Call Fun N times and print the total/min/avg/max/mdev runtime.
%%
%% The optional argument Formats is a list of result formats. The
%% options are 'stats' (text) and 'graph' (gnuplot).
dotimes(N, Fun) -> dotimes(N, Fun, [stats]).

dotimes(N, Fun, Formats) when integer(N), N > 0 ->
    Times = runtimes(N, Fun),
    foreach(fun(stats) -> print_stats(Times, ms);
	       (graph) -> plot(Times)
	    end, Formats).

%% Print 'ping'-style statistics about Values, a list of integers.
print_stats(Values) -> print_stats(Values, undefined).
print_stats(Values, Units) ->
    UnitsName = if Units == undefined -> "";
		   true               -> io_lib:format(" ~s", [Units])
		end,
    io:format("total = ~.3f~s, min/avg/max/mdev = ~.3f/~.3f/~.3f/~.3f~s~n",
	      [float(sum(Values)), UnitsName, float(min(Values)),
	       float(mean(Values)), float(max(Values)),float( mdev(Values)),
	       UnitsName]).

max(Values) -> lists:max(Values).
min(Values) -> lists:min(Values).
sum(Values) -> foldl(fun(X, Sum) -> X + Sum end, 0, Values).
mean(Values) -> sum(Values) / length(Values).
mdev(Values) ->
    M = mean(Values),
    sum([abs(V - M) || V <- Values]) / length(Values).

%% Call Fun N times and return the list of running times (float milliseconds).
runtimes(N, Fun) -> runtimes(N, Fun, []).

runtimes(0, F, Times) ->
    Times;
runtimes(N, F, Times) when N > 0 ->
    case timer:tc(erlang,apply,[F,[]]) of
	{_, {'EXIT',Reason}} ->
	    exit(Reason);
	{Time, _} ->
	    runtimes(N-1, F, [float(Time/1000)|Times])
    end.

%% Plot `Values' using gnuplot.
%% The vertical axis is the value and the horizontal is the value number.
plot(Values) ->
    Filename = fmt("/tmp/benchplot.~p.~p.~p", tuple_to_list(now())),
    {ok, Fd} = file:open(Filename, [write]),
    foreach(fun(V) -> io:format(Fd, "~p~n", [V]) end, Values),
    ok = file:close(Fd),
    Port = open_port({spawn, "gnuplot"}, []),
    erlang:port_command(Port, fmt("plot '~s' with line~n", [Filename])),
    io:get_line('Press return to continue.'),
    erlang:port_close(Port),
    ok = file:delete(Filename),
    ok.

fmt(F, A) -> lists:flatten(io_lib:format(F, A)).

