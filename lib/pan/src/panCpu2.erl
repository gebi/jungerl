%%%----------------------------------------------------------------------
%%% File    : panCpu2.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 10 Oct 2000 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(panCpu2).
-author('etxmacr@avc386').

-export([go/3]).

-define(CPUFORM, "~-14s~-30s~11w~11w~11w~n").
-define(RELTRIG, 0.2). %% T1/T2 > RELTRIG
-define(ABSTRIG, 100). %%    T1 > ABSTRIG

go(FD, F2, F1) ->
    do_io(FD, do(F2, F1)).

do_io(_, []) -> ok = io:fwrite("~p:~p - no cpu info yet~n", [?MODULE, ?LINE]);
do_io(FD, [{bef, Bs}, {aft, As}, {trig, Ts}, {same, Ss}]) ->
    io:fwrite(FD, ?CPUFORM, [pid, to_str(id), before, aftr, diff]),
    HR = lists:duplicate(72, $=),
    io:fwrite(FD, "Before ~s~n", [HR]), out(FD, Bs),
    io:fwrite(FD, "After =~s~n", [HR]), out(FD, As),
    io:fwrite(FD, "Triggd ~s~n", [HR]), out(FD, Ts),
    io:fwrite(FD, "Same ==~s~n", [HR]), out(FD, Ss),
    io:fwrite(FD, "=======~s~n", [HR]), sum(FD, Bs++As++Ts++Ss, 0, 0).
sum(FD, [], F, B) -> io:fwrite(FD, ?CPUFORM, [x, sum, F, B, F-B]);
sum(FD, [{_, _, F0, B0, _}|R], F, B) ->
    sum(FD, R, add(F, F0), add(B, B0)).
out(FD, []) -> ok;
out(FD, [{I, P, F, B, D}|R]) ->
    io:fwrite(FD, ?CPUFORM, [to_str(P), to_str(I), F, B, D]),
    out(FD, R).

do(F2, F1) ->
    panEts:new(panCpu2_in),
    panEts:new(panCpu2_out),
    lists:foreach(fun(E) -> ets_ins(panCpu2_in, E) end, makem(F1)),
    match(makem(F2)),
    [{T, all(T)} || T <- [bef, aft, trig, same]].

makem(no_file) -> [];
makem(F) ->
    panEts:new(panCpu2_tmp),
    panScan:file(F, '', {cb, cb_cpu, go, [panCpu2_tmp]}),
    strip(cb_cpu:summary(panCpu2_tmp)).

strip([]) -> [];
strip([E|T]) when tuple(E), size(E) == 6 -> [E|strip(T)];
strip([_|T]) -> strip(T).

all(Type) -> keysortr(key(Type), collapz(get_all(Type))).
key(bef) -> 3;
key(aft) -> 4;
key(_)   -> 5.
keysortr(N, L) -> lists:reverse(lists:keysort(N, L)).
get_all(Type) -> ets:match_object(panCpu2_out, {{Type,'_'},'_','_','_','_'}).
    
match([]) -> rest(ets_t2l(panCpu2_in));
match([{P, I, T1, In, Tgc, Gc}|R]) ->
    case filt(I) of
	true ->
	    case ets_lup(panCpu2_in, P) of
		[] -> ets_ins(panCpu2_out, {{bef, P}, I, T1, x, x});
		{P, I, T2, _, _, _} -> 
		    ets_del(panCpu2_in, P),
		    case trigger(T1, T2) of
		       true -> 
			    ets_ins(panCpu2_out, {{trig, P}, I, T1, T2, T1-T2});
		       _ -> 
			    ets_ins(panCpu2_out, {{same, P}, I, T1, T2, T1-T2})
		    end;
		{P, I2, T2, _, _, _} ->
		    ets_del(panCpu2_in, P),
		    io:fwrite("~p:~p: ~p ~p ~p~n", [?MODULE, ?LINE, P, I, I2])
	    end;
	_ -> ok
    end,
    match(R).

trigger(T1, T2) -> 
    case {abs((T1-T2)/T1) > ?RELTRIG, T1 > ?ABSTRIG} of
	{true, true} -> true;
	_ -> false
    end.

rest([]) -> [];
rest([{P, I, T2, In, Tgc, Gc}|R]) -> 
    ets_del(panCpu2_in, P),
    case filt(I) of
	true -> ets_ins(panCpu2_out, {{aft, P}, I, x, T2, x});
	_ -> ok
    end,
    rest(R).

%%%filt({erlang,apply,2}) -> false;
filt(file_server) -> false;
%%%filt({sysTimer,_,_}) -> false;
filt({user_drv,server,2}) -> false;
filt({group,server,2}) -> false;
%%%filt({inet_tcp_dist,do_accept,6}) -> false;
%%%filt({net_kernel,ticker,2}) -> false;
filt({shell,server,1}) -> false;
filt(_) -> true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collapz(L) -> 
    panEts:new(panCpu_coll), 
    collapse(L).
collapse([]) -> ets_t2l(panCpu_coll);
collapse([{{_, P}, I, F, B, D}|R]) -> 
    case ets_lup(panCpu_coll, I) of
	[] -> ets_ins(panCpu_coll, {I, P, F, B, D});
	{I, N, F0, B0, D0} when integer(N) -> 
	    Item = {I, N+1, add(F0, F), add(B0, B), add(D0, D)},
	    ets_ins(panCpu_coll, Item);
	{I, _, F0, B0, D0} -> 
	    Item = {I, 2, add(F0, F), add(B0, B), add(D0, D)},
	    ets_ins(panCpu_coll, Item)
    end,
    collapse(R).

add(Y, X) when integer(X), integer(Y) -> X+Y;
add(A, _) -> A.
to_str(I) when integer(I) -> integer_to_list(I);
to_str(A) when atom(A) -> atom_to_list(A);
to_str(L) when list(L) -> L;
to_str(T) -> io_lib:fwrite("~w", [T]).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_ins(Tab, Rec) ->
    catch ets:insert(Tab, Rec).
ets_del(Tab, Key) ->
    catch ets:delete(Tab, Key).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	{'EXIT', _} -> [];
	[R] -> R;
	[] -> []
    end.
ets_t2l(Tab) ->
    case catch ets:tab2list(Tab) of
	{'EXIT', _} -> [];
	L -> L
    end.
