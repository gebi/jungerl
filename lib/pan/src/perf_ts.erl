%%%-------------------------------------------------------------------
%%% File    : perf_ts.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created : 28 Nov 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(perf_ts).

-export([go/0, go/1]).

-define(CB, cb_cpu).
-define(PID(I), c:pid(0,127,I)).
-define(FDMSG(Tag, US), 
	[{Tag,{pid,foo},0,{0,0,US}},1,standard_io]++stat()).
-define(MSG(Tag, Pid, US), 
	[{Tag,{?PID(Pid),foo},info,{0,0,US}},1,standard_io]++stat()).
-define(FDEVENT(Tag, US), put(state, ?CB:go(?FDMSG(Tag, US)))).
-define(EVENT(Tag, Pid, US), put(state, ?CB:go(?MSG(Tag, Pid, US)))).
-define(END, ?CB:go([end_of_trace, 1, standard_io]++stat())).

go() -> do(1).
go(N) -> 
    pre(N),
    tc(N).

do(N) ->
    case go(N) of
	not_yet_implemented -> the_end;
	X -> do(N+1)
    end.

pre(N) ->
    io:fwrite("##  ~p  ######~n", [N]),
    catch ets:delete(?MODULE),
    ets:new(?MODULE, [named_table,public,ordered_set]),
    erase(state).

stat() ->
    case get(state) of
	undefined -> [initial, ?MODULE];
	S -> [S]
    end.
	     
post(Conds) ->
    post(Conds, {ets:tab2list(?MODULE), 0}).
post([], {[], 0}) -> io:fwrite("ok~n");
post([], {[{total, T}], T}) -> io:fwrite("ok~n");
post([], X) -> io:fwrite("failed~n~p~n", [X]);
post([{P, Con}|Conds], {D, T}) ->
    post(Conds, do_post(P, Con, D, T)).
do_post(P, [], D, T) -> {D, T};
do_post(P = file_driver, [{Tag, V}|Cons], D, T) ->
    do_post(P, Cons, D--[{{P,Tag},V}], T);
do_post(P, [{Tag,V}|Cons], D, T) when Tag == cpu; Tag == gcpu->
    do_post(P, Cons, D--[{{?PID(P),Tag},V}], T+V);
do_post(P, [{Tag,V}|Cons], D, T) ->
    do_post(P, Cons, D--[{{?PID(P),Tag},V}], T).

tc(1) ->
    ?EVENT(in, 1, 0),
    ?EVENT(out, 1, 3),
    ?END,
    post([{1, [{cpu,3},{in,1}]}]);
tc(2) ->
    ?EVENT(in, 1, 0),
    ?EVENT(gc_start, 1, 2),
    ?EVENT(gc_end, 1, 3),
    ?EVENT(out, 1, 7),
    ?END,
    post([{1, [{in,1},{gc,1},{cpu,6},{gcpu,1}]}]);
tc(3) ->
    ?EVENT(in, 1, 0),
    ?EVENT(gc_start, 2, 3),
    ?EVENT(gc_end, 2, 4),
    ?EVENT(out, 1, 7),
    ?END,
    post([{1, [{in,1},{cpu,6}]},
	  {2, [{gc,1},{gcpu,1}]}]);
tc(4) ->
    ?EVENT(in, 1, 0),
    ?EVENT(out, 1, 7),
    ?EVENT(gc_start, 2, 8),
    ?EVENT(gc_end, 2, 10),
    ?EVENT(in, 3, 12),
    ?EVENT(out, 3, 17),
    ?END,
    post([{1, [{in,1},{cpu,7}]}, 
	  {2, [{gc,1},{gcpu,2}]}, 
	  {3, [{in,1},{cpu,5}]}]);
tc(5) ->
    ?EVENT(in, 1, 0),
    ?EVENT(out, 1, 3),
    ?EVENT(gc_start, 2, 5),
    ?FDEVENT(out, 7),
    ?FDEVENT(in, 8),
    ?EVENT(gc_end, 2, 10),
    ?EVENT(in, 1, 12),
    ?EVENT(out, 1, 17),
    ?END,
    post([{1, [{in,2},{cpu,8}]}, 
	  {2, [{gc,1},{gcpu,4}]},
	  {file_driver, [{in,1},{cpu,1}]}]);
tc(6) ->
    ?EVENT(in, 1, 0),
    ?EVENT(gc_start, 2, 5),
    ?FDEVENT(out, 7),
    ?FDEVENT(in, 8),
    ?EVENT(gc_end, 2, 10),
    ?EVENT(out, 1, 17),
    ?END,
    post([{1, [{in,1},{cpu,12}]}, 
	  {2, [{gc,1},{gcpu,4}]},
	  {file_driver, [{in,1},{cpu,1}]}]);
tc(7) ->
    ?EVENT(in, 1, 0),
    ?FDEVENT(out, 7),
    ?FDEVENT(in, 8),
    ?EVENT(out, 1, 17),
    ?END,
    post([{1, [{in,1},{cpu,16}]},
	  {file_driver, [{in,1},{cpu,1}]}]);
tc(8) ->
    ?EVENT(gc_start, 2, 3),
    ?EVENT(gc_end, 2, 4),
    ?END,
    post([{2, [{gc,1},{gcpu,1}]}]);
tc(9) ->
    ?EVENT(gc_end, 2, 4),
    ?END,
    post([]);
tc(10) ->
    ?FDEVENT(in, 8),
    ?EVENT(gc_end, 2, 4),
    ?EVENT(out, 1, 17),
    ?END,
    post([]);
tc(11) ->
    ?FDEVENT(in, 2),
    ?EVENT(gc_end, 2, 4),
    ?EVENT(out, 1, 17),
    ?EVENT(in, 2, 23),
    ?EVENT(gc_start, 2, 25),
    ?FDEVENT(out, 27),
    ?END,
    post([{file_driver,[{in,1}]},
	  {2,[{gc,1},{in,1}]}]);
tc(12) ->
    ?FDEVENT(in, 2),
    ?EVENT(gc_end, 2, 4),
    ?EVENT(out, 1, 17),
    ?EVENT(in, 3, 18),
    ?EVENT(out, 3, 21),
    ?EVENT(in, 2, 23),
    ?EVENT(gc_start, 2, 25),
    ?FDEVENT(out, 27),
    ?END,
    post([{file_driver,[{in,1}]},
	  {2,[{gc,1},{in,1}]},
	  {3,[{in,1},{cpu,3}]}]);
tc(13) ->
    ?EVENT(in, 1, 0),
    ?EVENT(out, 1, 3),
    ?EVENT(exit, 1, 6),
    ?END,
    post([{1, [{cpu,3},{in,1}]}]);
tc(14) ->
    ?EVENT(in, 1, 0),
    ?EVENT(exit, 1, 6),
    ?END,
    post([{1, [{cpu,6},{in,1}]}]);
tc(_) -> not_yet_implemented.
