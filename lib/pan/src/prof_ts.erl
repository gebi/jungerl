%%%-------------------------------------------------------------------
%%% File    : prof_ts.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created : 28 Nov 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(prof_ts).

-export([go/0, go/1]).

-define(CB, cb_prof).
-define(PID(I), if integer(I) -> c:pid(0,127,I); true -> pid end).
-define(MSG(Tag, Pid, MFA, US),
	[{Tag,{?PID(Pid),foo},MFA,{0,0,US}},1,standard_io]++stat()).
-define(FDEVENT(Tag, US), put(state, ?CB:go(?MSG(Tag, pid, 0, US)))).
-define(CEVENT(Tag, Pid,F, US), put(state, ?CB:go(?MSG(Tag, Pid,{m,F,0}, US)))).
-define(XEVENT(Tag, Pid,X, US), put(state, ?CB:go(?MSG(Tag, Pid,X, US)))).
-define(EVENT(Tag, Pid, US), put(state, ?CB:go(?MSG(Tag, Pid, info,US)))).
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
    case post(Conds, [])--Conds of
	[] -> io:fwrite("ok~n");
	X -> io:fwrite("failed~n~p~n~p~n", [Conds, X])
    end.    
post([], O) -> O;
post([{Pid,Cs}|Conds], O) ->
    Ds = ets:match(?MODULE,{{{stack,time},?PID(Pid),'$1'},'$2'}),
    post(Conds, [{Pid,do_post(Ds, [])}|O]).
do_post([], O) -> lists:reverse(O);
do_post([[Stack,Time]|Ds], O) ->
    do_post(Ds, [{st(Stack),Time}|O]).
st(St) -> [F||{m,F,0}<-St].

tc(1) ->
    ?CEVENT(in, 1, f0, 0),
    ?CEVENT(call, 1, f1, 3),
    ?CEVENT(call, 1, f2, 4),
    ?CEVENT(return_to, 1, f1, 5),
    ?EVENT(out, 1, 7),
    ?END,
    post([{1, [{[f0],3},{[f0,f1],3},{[f0,f1,f2],1}]}]);
tc(2) ->
    ?CEVENT(in, 1, f0, 0),
    ?EVENT(gc_start, 1, 2),
    ?EVENT(gc_end, 1, 3),
    ?EVENT(out, 1, 7),
    ?END,
    post([{1, [{[f0],6}]}]);
tc(3) ->
    ?CEVENT(in, 1, f0, 0),
    ?EVENT(gc_start, 2, 3),
    ?EVENT(gc_end, 2, 4),
    ?EVENT(out, 1, 7),
    ?END,
    post([{1, [{[f0],6}]}]);
tc(4) ->
    ?CEVENT(in, 1, f0, 0),
    ?EVENT(out, 1, 7),
    ?EVENT(gc_start, 2, 8),
    ?EVENT(gc_end, 2, 10),
    ?CEVENT(in, 3, f0, 12),
    ?CEVENT(call, 3, f1, 14),
    ?EVENT(out, 3, 17),
    ?END,
    post([{1, [{[f0],7}]}, 
	  {3, [{[f0],2},{[f0,f1],3}]}]);
tc(5) ->
    ?CEVENT(in, 1, f0, 0),
    ?CEVENT(call, 1, f1, 2),
    ?EVENT(out, 1, 3),
    ?EVENT(gc_start, 2, 5),
    ?FDEVENT(out, 7),
    ?FDEVENT(in, 8),
    ?EVENT(gc_end, 2, 10),
    ?CEVENT(in, 1, f1, 12),
    ?CEVENT(return_to, 1, f0, 14),
    ?EVENT(out, 1, 17),
    ?END,
    post([{1, [{[f0],5},{[f0,f1],3}]}]);
tc(6) ->
    ?CEVENT(in, 1, f0, 0),
    ?EVENT(gc_start, 2, 5),
    ?FDEVENT(out, 7),
    ?FDEVENT(in, 8),
    ?EVENT(gc_end, 2, 10),
    ?EVENT(out, 1, 17),
    ?END,
    post([{1, [{[f0],12}]}]);
tc(7) ->
    ?CEVENT(in, 1, f0, 0),
    ?FDEVENT(out, 7),
    ?FDEVENT(in, 8),
    ?EVENT(out, 1, 17),
    ?END,
    post([{1, [{[f0],16}]}]);
tc(8) ->
    ?EVENT(gc_start, 2, 3),
    ?EVENT(gc_end, 2, 4),
    ?END,
    post([]);
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
    ?CEVENT(in, 2, f0, 23),
    ?EVENT(gc_start, 2, 25),
    ?FDEVENT(out, 27),
    ?END,
    post([{2,[{[f0],2}]}]);
tc(12) ->
    ?FDEVENT(in, 2),
    ?EVENT(gc_end, 2, 4),
    ?EVENT(out, 1, 17),
    ?CEVENT(in, 3, f0, 18),
    ?EVENT(out, 3, 21),
    ?CEVENT(in, 2, f2, 23),
    ?EVENT(gc_start, 2, 25),
    ?FDEVENT(out, 27),
    ?END,
    post([{3,[{[f0],3}]},
	  {2,[{[f2],2}]}]);
tc(13) ->
    ?CEVENT(in, 1, f0, 0),
    ?EVENT(out, 1, 3),
    ?EVENT(exit, 1, 6),
    ?END,
    post([{1, [{[f0],3}]}]);
tc(14) ->
    ?CEVENT(in, 1, f0, 0),
    ?EVENT(exit, 1, 6),
    ?END,
    post([{1, [{[f0],6}]}]);
tc(15) ->
    ?CEVENT(in, 1, f0, 0),
    ?EVENT(exit, 1, 6),
    ?EVENT(gc_start, 3, 12),
    ?EVENT(gc_end, 3, 14),
    ?CEVENT(in, 2, f2, 18),
    ?EVENT(out, 2, 25),
    ?END,
    post([{1, [{[f0],6}]},
	  {2, [{[f2],7}]}]);
tc(_) -> not_yet_implemented.
