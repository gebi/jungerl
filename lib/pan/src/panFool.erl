%%%-------------------------------------------------------------------
%%% File    : panFool.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created : 14 Jan 2003 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(panFool).
-export([go/0,go/1,go/2,go/3]).

-define(CALLBACK, cb_foolprof).

go() -> procs().
go(Proc) -> go(Proc, []).
go(Proc, Prefix) -> go(Proc, Prefix, -1).
go(Proc, Prefix, Count) ->
    Pid = key(Proc),
    Tot = ets_lup(?CALLBACK, {total, time}),
    Ptot = ets_lup(?CALLBACK, {{pid, time}, Pid}),
    stax(Count, 0, Tot, Pid, Prefix, ets:next(?CALLBACK, {{stack,time},Pid,0})).

stax(M, M, _, Pid, _, {_,Pid,_}) -> {did, M, stacks};
stax(M, N, Tot, Pid, Prefix, {_,Pid,_} = Key) -> 
    {_, Stak, Time} = ets_lup(?CALLBACK, Key),
    case prefix(lists:reverse(Stak), Prefix) of
	{yes, Tail} -> io:fwrite("~6w ~w~n", [N, Tail]);
	no -> ok
    end,
    stax(M, N+1, Tot, Pid, Prefix, ets:next(?CALLBACK, Key));
stax(_, _, _, Pid, _, _) -> {Pid, exhausted}.

prefix(Rest, '') -> prefix(Rest, []);
prefix(Rest, []) -> {yes, Rest};
prefix([MFA|Rest], [MFA|Tail]) -> prefix(Rest, Tail);
prefix(_, _) -> no.

procs() ->
    Tot = ets_lup(?CALLBACK, {total, time}),
    TPs = ets:match(?CALLBACK, {{{pid,time},'$2'},'$1'}),
    RTs = [{reg(P), T} || [T, P] <- TPs],
    F = fun({R, T}, [{R, N, TT}|O]) -> 
		[{R, N+1, percent(T, Tot)+TT}|O];
	   ({R, T}, O) ->
		[{R, 1, percent(T, Tot)}|O]
	end,
    RNTs = lists:foldl(F, [], lists:sort(RTs)),
    [[length(TPs), Tot]|rsort(3, RNTs)].

reg(P) -> ets_lup(panScan, {P,registered_name}).
rsort(Pos, L) -> lists:reverse(lists:keysort(Pos, L)).
percent(A, 0) -> 0;
percent(A, B) -> A/B*100.

key('') -> '';
key(Reg) when atom(Reg) ->
    key([Reg, 1]);
key(MFA) when tuple(MFA) -> 
    key([MFA, 1]);
key([PID, N]) ->
    Pids = ets:match(panScan, {{'$1',registered_name},PID}),
    case length(Pids) of
	0 ->
	    exit({{?MODULE, ?LINE}, no_such_process, PID});
	L when L < N -> 
	    io:fwrite("only ~w procs with this tag~n", [length(Pids)]),
	    key({PID, 1});
	_ ->
	    [Pid] = lists:nth(N, Pids),
	    Pid
    end.
ets_lup(Tab, Tag) ->
    case catch ets:lookup(Tab, Tag) of
	[{Tag, Val}] -> Val;
	{'EXIT',_} -> [];
	[X] -> X
    end.
