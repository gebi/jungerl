%%%-------------------------------------------------------------------
%%% File    : edit_bench.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Random benchmarking
%%%
%%% Created : 29 Sep 2001 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(edit_bench).

-compile(export_all).

%% Testing the speed of sending (large) cords in messages

cord_bench(Filename, N) ->
    {ok, Cord} = cord:new_from_file(Filename),
    Pid = spawn_link(?MODULE, cord_receiver, []),
    timer:tc(?MODULE, cord_bench_loop, [Cord, Pid, N]).

cord_bench_loop(Cord, Pid, 0) ->
    exit(Pid, kill),
    ok;
cord_bench_loop(Cord, Pid, N) when N > 0 ->
    Pid ! {cord, self(), Cord},
    receive ack -> ok end,
    cord_bench_loop(Cord, Pid, N-1).

cord_receiver() ->
    receive {cord, Who, Cord} -> Who ! ack end,
    cord_receiver().



