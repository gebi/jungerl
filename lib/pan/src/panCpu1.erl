%%%----------------------------------------------------------------------
%%% File    : panCpu1.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 10 Oct 2000 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(panCpu1).
-author('etxmacr@avc386').

-export([go/2]).

hr(excel) -> [];
hr(wide) -> lists:duplicate(131, $=)++[10];
hr(normal) -> lists:duplicate(79, $=)++[10].
format(excel) -> "~-14s~-80s~7w~10w~7w~10w~n";
format(wide) -> "~-14s~-93s~7w~7w~10w~n";
format(normal) -> "~-14s~-41s~7w~7w~10w~n".
tags(excel) -> [to_str(pid), to_str(id), gc, gcpu, in, cpu];
tags(_) -> [to_str(pid), to_str(id), gc, in, cpu].
sums(FD,excel,P,C,G)->io:fwrite(FD,format(excel),[to_str(P),to_str(x),x,G,x,C]);
sums(FD,O,P,C,G) -> io:fwrite(FD,format(O),[to_str(P),to_str(x),x,x,C]).

go(FD_O, File) ->
    do_io(FD_O, collapz(makem(File))).

do_io(_, []) -> ok = io:fwrite("~p:~p - no cpu info yet~n", [?MODULE, ?LINE]);
do_io({FD, O} = FD_O, L) ->
    io:fwrite(FD, format(O), tags(O)),
    io:fwrite(FD, hr(O), []), out(FD_O, L),
    io:fwrite(FD, hr(O), []), sum(FD_O, L, 0, 0, 0).

sum({FD, O} = FD_O, [], P, C, G) -> 
    sums(FD,O,P,C,G),C;
sum(FD_O, [{Pid, _, C0, _, G0, _}|R], P, C, G) ->
    sum(FD_O, R, add(P, Pid), add(C, C0), add(G, G0)).

out(FD_O, []) -> ok;
out({FD, O} = FD_O, [{P, I, Ttot, In, Tgc, Gc}|R]) when O == excel->
    io:fwrite(FD, format(O), [to_str(P), to_str(I), Gc, Tgc, In, Ttot]),
    out(FD_O, R);
out({FD, O} = FD_O, [{P, I, Ttot, In, Tgc, Gc}|R]) ->
    io:fwrite(FD, format(O), [to_str(P), to_str(I), Gc, In, Ttot]),
    out(FD_O, R).

makem(no_file) -> [];
makem(F) ->
    panScan:file(F, '', cb_perf),
    strip(summary(cb_perf)).

strip([]) -> [];
strip([E|T]) when tuple(E), size(E) == 6 -> [E|strip(T)];
strip([_|T]) -> strip(T).

summary(Tab) -> 
    Fun = 
	fun([P, T]) -> 	
		ets_ins(Tab, {{P, info}, panScan:proc_tag(P)}) 
	end,
    lists:foreach(Fun,  ets_mch(Tab, {{'$1', cpu}, '$2'})),
    summary(Tab, 2, [info, cpu, in, gcpu, gc]).
summary(Tab, N, Tags) -> 
    [[pid|Tags]|lists:reverse(lists:keysort(N+1, do_sum(Tab, Tags)))].

do_sum(Tab, [Tag|Tags]) -> 
    do_sum(Tab, Tags, ets_mch(Tab, {{'$1', Tag}, '$2'})).
do_sum(Tab, _, []) -> [];
do_sum(Tab, Tags, [[Pid, I1]|T]) -> 
    [list_to_tuple([Pid, I1|tail(Tab, Pid, Tags)])|do_sum(Tab, Tags, T)].
tail(Tab, _, []) -> [];
tail(Tab, Pid, [H|T]) -> 
    [case ets_lup(Tab, {Pid, H}) of 
	 [] -> 0; 
	 X -> X 
     end|tail(Tab, Pid, T)].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collapz(L) -> 
    panEts:new(?MODULE, [{keypos, 2}]), 
    keysortr(3, collapse(L)).
collapse([]) -> ets_t2l(?MODULE);
collapse([Obj|R]) -> 
    case filt(Obj) of
	true -> ets_ins(?MODULE, obj(Obj));
	_ -> ok
    end,
    collapse(R).

obj({P, I, Tin, In, Tgc, Gc} = Obj) ->
    case ets_lup(?MODULE, I) of
	[] -> 
	    {P, I, add(Tin,Tgc), In, Tgc, Gc};
	{N, I, Tin0, In0, Tgc0, Gc0}-> 
	    upd_obj(N, P, I, Tin, Tin0, Tgc, Tgc0, In, In0, Gc, Gc0)
    end.
upd_obj(N, P, I, Tin, Tin0, Tgc, Tgc0, In, In0, Gc, Gc0) ->
    Ttot = add(Tgc, add(Tin, Tin0)),
    {add(N, P), I, Ttot, add(In, In0), add(Tgc, Tgc0), add(Gc, Gc0)}.

add(Y, X) when integer(X), integer(Y) -> X+Y;
add(Pi, Pid) when pid(Pi), pid(Pid) -> 2;
add(P, Pid) when pid(Pid) -> add(P, 1);
add(A, _) -> A.

keysortr(N, L) -> lists:reverse(lists:keysort(N, L)).
    
filt({P, I, Tin, In, Tgc, Gc}) -> filt(I);
%%%filt({erlang,apply,2}) -> false;
filt(file_server) -> false;
%%%filt({sysTimer,_,_}) -> false;
filt({user_drv,server,2}) -> false;
filt({group,server,2}) -> false;
%%%filt({inet_tcp_dist,do_accept,6}) -> false;
%%%filt({net_kernel,ticker,2}) -> false;
filt({shell,server,1}) -> false;
filt(_) -> true.

to_str(I) when integer(I) -> integer_to_list(I);
to_str(A) when atom(A) -> atom_to_list(A);
to_str(L) when list(L) -> L;
to_str(T) -> io_lib:fwrite("~w", [T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_ins(Tab, Rec) ->
    catch ets:insert(Tab, Rec).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	{'EXIT', _} -> [];
	[{Key, R}] -> R;
	[R] -> R;
	[] -> []
    end.
ets_t2l(Tab) ->
    case catch ets:tab2list(Tab) of
	{'EXIT', _} -> [];
	L -> L
    end.
ets_mch(Tab, Pat) ->
    case catch ets:match(Tab, Pat) of
	{'EXIT', _} -> [];
	L -> L
    end.
