%%%----------------------------------------------------------------------
%%% File    : panProf.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 24 Aug 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(panProf).
-author('etxmacr@avc386').

-export([proc/4]).
-export([tree/1]).
-export([distel/1]).

-define(CALLBACK, cb_prof).

proc(Dir, Proc, nostack, File) ->
    proc([Dir, Proc, File]);
proc(Dir, Proc, Stack, File) ->
    proc([Dir, Proc, Stack, File]).

proc(Args) ->
    case catch do_proc(Args) of
	{'EXIT', R} -> io:fwrite("*** caught ~p~n", [R]);
	_ -> ok
    end.

do_proc([Dir, Proc, Stack, File]) ->
    {FD, Typ} = open(File),
    produce(Dir),
    stack(FD, Proc, Stack),
    close(FD);
do_proc([Dir, Proc, File]) ->
    {FD, Typ} = open(File),
    produce(Dir),
    out(FD, Typ, Proc, procs(key(Proc))),
    close(FD).

-define(SFORM, "~-56s~12s~10s%~n").
-define(OFORM, "   , ~-56.56..w~7w~10.1f%~n").
-define(STFORM, "~-61.61..w~7w~10.1f%~n").

stack(Out, PI, '') -> stack(Out, PI, top_stack(key(PI), []));
stack(Out, PI, MFA) when tuple(MFA) -> 
    SCs = ets:match(?CALLBACK, {{{stack,calls},key(PI),'$1'},'$2'}),
    CSs = [[C, S] || [S, C] <- SCs, lists:member(MFA,S)],
    lists:foreach(fun(CS) -> io:fwrite("=== ~w~n~p~n", CS) end, lists:sort(CSs));
stack(Out, PI, Stack) ->
    Key = key(PI),
    io:fwrite(Out, ?SFORM, ["MFA", "calls", "cpu"]),
    clr_chk(),
    Tot = tot(Key),
    Childs = childs(Key, Stack),
    stack_out(Key, Stack, [], Tot),
    lists:foreach(fun({MFA, T}) -> cfun(Key, MFA, T, Tot, Stack) end, Childs),
    io:fwrite(Out, ?SFORM, ["total", "", io_lib:fwrite("~5.1f", [get_chk()])]).

top_stack(PI, Stack) ->
    case childs(PI, Stack) of
	[] -> Stack;
	[{MFA, T}|_] -> top_stack(PI, Stack++[MFA])
    end.

tot(Key) ->
    ets_lup(?CALLBACK, {{pid, time}, Key}).

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

cfun(Key, MFA, T, Tot, Stack) ->
    {T0, C} = funi(Key, Stack++[MFA]),
    io:fwrite(?OFORM, [MFA, C, percent(T, Tot)]).

stack_out(Key, [], _, Tot) -> ok;
stack_out(Key, [MFA|ST], STO, Tot) ->
    NSTO = STO++[MFA],
    {T, C} = funi(Key, NSTO),
    io:fwrite(?STFORM, [MFA, C, percent(T, Tot)]),
    stack_out(Key, ST, NSTO, Tot).

funi(Key, St) ->
    case [ets_lup(?CALLBACK, {{stack,CT},Key,St}) || CT <- [calls, time]] of
	[[], []] -> exit({{?MODULE, ?LINE}, bad_stack, St});
	[[], T] -> {T, 0};
	[C, T] -> {T, C}
    end.

childs(Key, Stack) ->
    CStack = Stack++['$1'|'_'],
    cum_childs(ets:match(?CALLBACK, {{{stack,time},Key,CStack}, '$2'})).

cum_childs([[MFA, _]|_] = L) -> cum_childs(L, [{MFA, 0}]);
cum_childs([]) -> [].
     
cum_childs([[MFA, T]|Tail], [{MFA, T0}|TailO]) ->
    cum_childs(Tail, [{MFA, T0+T}|TailO]);
cum_childs([[MFA, T]|Tail], TailO) ->
    cum_childs(Tail, [{MFA, T}|TailO]);
cum_childs([], TailO) ->
    lists:reverse(lists:keysort(2, TailO)).


produce('') ->
    case ets_lup(?CALLBACK, file) of
	[] -> 
	    case file:open(FN = atom_to_list(?CALLBACK)++".ets", []) of
		{ok, FD} -> 
		    file:close(FD),
		    io:fwrite("loading ~s...", [FN]),
		    Self = self(),
		    F = fun() -> 
				register(?CALLBACK,self()), 
				ets:file2tab(FN),
				Self ! {ok, ?CALLBACK},
				receive quit -> ok end 
			end,
		    spawn_link(F),
		    receive {ok, ?CALLBACK} -> ok end,
		    io:fwrite(".. done~n", []);
		_ ->
		    exit({{?MODULE, ?LINE}, no_data_no_file})
	    end;
	File ->
	    io:fwrite("using data from ~p~n", [File])
    end;
produce(Dir) ->
    File = get_file(Dir),
    case File == ets_lup(?CALLBACK, file) of
	true -> ok;
	false -> init(File)
    end.

get_file(Dir) ->
    case panScan:check_file(Dir) of
	{ok, File} -> File;
	R -> exit({no_file, R})
    end.

init(File) ->
    pan:scan(File, '', {cb, ?CALLBACK}),
    ets:insert(?CALLBACK, {file, File}).

out(Out, Typ, '', [[Np, TotUs, Chk]|Ps]) ->
    Us = 1.0,%%should be wall clock time
    io:fwrite(Out, form(head, Typ), [all, Np, TotUs, Chk]),
    io:fwrite(Out, form(tags, Typ), ["process tag", "procs", "cpu%"]),
    F = fun({Pi, Nps, Pct}) -> 
		io:fwrite(Out, form(bred, Typ), [str(Pi), Nps, Pct])
	end,
    lists:foreach(F, Ps);
out(Out, Typ, Proc, [[Nmfa, TotUs, Chk]|MFAs]) ->
    io:fwrite(Out, form(head, Typ), [Proc, Nmfa, TotUs, Chk]),
    io:fwrite(Out, form(tags, Typ), ["MFA", "calls", "cpu%"]),
    F = fun({MFA, Pct, Nc}) -> 
		io:fwrite(Out, form(bred, Typ), [str(MFA), Nc, Pct])
	end,
    lists:foreach(F, MFAs).
str(Term) ->
    lists:flatten(io_lib:fwrite("~p", [Term])).

form(head, term) -> "\"~p; N = ~p, ~p us (~p)\".~n";
form(tags, term) -> "\"{~s, ~s, ~s}\".~n";
form(bred, term) -> "{~s,~w,~w}.~n";
form(head, _) -> "~p; N = ~p, ~p us (~p)~n";
form(tags, _) -> "  ~-60s~8s~8s~n";
form(bred, _) -> "  ~-60.60..s~8w~8.1f~n".

procs('') ->
    Tot = ets_lup(?CALLBACK, {total, time}),
    TPs = ets:match(?CALLBACK, {{{pid,time},'$2'},'$1'}),
    RTs = [{reg(P), T} || [T, P] <- TPs],
    clr_chk(),
    F = fun({R, T}, [{R, N, TT}|O]) -> 
		[{R, N+1, percent(T, Tot)+TT}|O];
	   ({R, T}, O) ->
		[{R, 1, percent(T, Tot)}|O]
	end,
    RNTs = lists:foldl(F, [], lists:sort(RTs)),
    [[length(TPs), Tot, get_chk()]|rsort(3, RNTs)];
procs(Key) ->
    Tot = ets_lup(?CALLBACK, {{pid,time},Key}),
    TFs = lists:sort(ets:match(?CALLBACK, {{{func,time},Key,'$2'}, '$1'})),
    clr_chk(),
    FTCs = lists:reverse([{F, percent(T, Tot), c_lup(Key, F)} || [T, F] <- TFs]),
    [[length(TFs), Tot, get_chk()]|FTCs].

c_lup(Key, MFA) -> 
    case ets_lup(?CALLBACK, {{func,calls},Key,MFA}) of
	[] -> 0;
	I -> I
    end.
reg(P) -> ets_lup(panScan, {P,registered_name}).

percent(A, 0) -> 0;
percent(A, B) -> Q = A/B*100, upd_chk(Q), Q.

rsort(Pos, L) -> lists:reverse(lists:keysort(Pos, L)).

open('') -> {standard_io, text};
open(File) when list(File) ->
    {ok, FD} = file:open(File, [write]),
    {FD, type(lists:reverse(File))}.
type("mret"++X) -> term;
type(_) -> text.

close(FD) when pid(FD) -> file:close(FD);
close(A) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface function to treebrowser by etxpell
%%% {procs, Tag, Pct, [[N, Pct],...]}
%%% {proc, Tag, Pct}
%%% {mfa, Tag, Stack, T}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tree({file, File}) ->
    case File of
	"" -> produce('');
	_ -> produce(File)
    end,
    [[Np, TotUs, Chk]|T] = procs(''),
    [case Nps of 
	 1 -> {proc, Tag, Pct};
	 _ -> {procs, Tag, Pct, tprocs(Tag, Nps, TotUs)}
     end || {Tag, Nps, Pct, Ns} <- T];
tree({children, Tag, Stack}) ->
    Key = key(Tag),
    Tot = tot(Key),
    Chs = childs(Key, Stack),
    [{mfa, Tag, Stack++[MFA], percent(T, Tot)} || {MFA, T} <- Chs];
tree({children, Tag}) ->
    tree({children, Tag, []}).

tprocs(Tag, Nps, TotUs) ->
    lists:keysort(2, [{N, perc([Tag, N], TotUs)} || N <- lists:seq(1, Nps)]).
perc(Key, Tot) -> percent(tot(key(Key)), Tot).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%interface function to distel's pan.el
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
distel(File) ->
    case File of
	"" -> produce('');
	_ -> produce(File)
    end,
    [[Np, TotUs, Chk]|T] = procs(''),
    [{flatform([5,-50,5], [Nps, Tag, Pct]), Tag} || {Tag, Nps, Pct, Ns} <- T].

flatform(Lens, Trms) -> flatform(Lens, Trms, [], []).
flatform([], [], Ol, Ot) -> lists:flatten(io_lib:fwrite(Ol++"~n", Ot));
flatform([Len|Lens], [Trm|Trms], Ol, Ot) -> 
    flatform(Lens, Trms,Ol++"~"++to_list(Len)++"s ",
	     Ot++io_lib:format("~p",[Trm])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_lup(Tag) -> ets_lup(?CALLBACK, Tag).
ets_lup(Tab, Tag) ->
    case catch ets:lookup(Tab, Tag) of
	[{Tag, Val}] -> Val;
	{'EXIT',_} -> [];
	X -> X
    end.
ets_upd(Key, Inc) ->
    case catch ets:update_counter(?CALLBACK, Key, Inc) of
        {'EXIT', _ } -> 
	    ets:insert(?CALLBACK, {Key, Inc}), 
	    0;
        Old -> Old
    end.
ets_clr(Tag) ->
    ets:insert(?CALLBACK, {Tag, 0}).

to_list(I) when integer(I) -> integer_to_list(I).
    
clr_chk() -> ets_clr(chk).
get_chk() -> ets_lup(chk)/1000.
upd_chk(Q) -> ets_upd(chk, round(Q*1000)).
