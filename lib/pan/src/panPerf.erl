%%%-------------------------------------------------------------------
%%% File    : panPerf.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created : 29 Nov 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(panPerf).
-export([go/2]).

-define(ETSFORM, "~-5w~-13w~-29s~8w~7w~9w~8w~n").
-define(EXCLTABS, [mnesia_gvar, mnesia_transient_decision]).
-define(FILETYPES, [{trc, "-[0-9]*-[0-9]*\.trc\$"}]).

go(Dir, '') ->
    summ(Dir);
go(Dir, File) ->
    open(File),
    summ(Dir),
    close().
summ(Dir) ->
    {Rdir, Rfiles} = 
	case filename:basename(Dir) of
	    Nod = "axd301@cp"++T -> {filename:dirname(Dir), {ok, [Nod]}};
	    _ -> {Dir, file:list_dir(Dir)}
	end,
    do_summ(Rdir, Rfiles), ok.

do_summ(Top, {ok, Nods}) -> 
    [do_summ(Top, Nod, files(Top, Nod)) || Nod <- lists:sort(Nods)];
do_summ(Top, Err) -> io:fwrite("~p: in ~p: error ~p~n", [?MODULE, Top, Err]).
do_summ(Top, Nod, {ok, Files}) -> 
    Trc = trcfiles(Nod, Files),
    dout( "~s~n~p: ~s/~s~n", [hr(), ?MODULE, Top, Nod]),
    CpuTime = cpui(Trc),
    dout( "~s~n", [hr()]),
    sysi(CpuTime),
    etsi().

files(Top, Nod) -> 
    case file:list_dir(Dir = filename:join([Top, Nod])) of
	{ok, Fs} -> {ok, [filename:join([Dir, F]) || F <- Fs]};
	Err -> exit({?MODULE, bad_dir, Dir, Err})
    end.

hr() -> lists:duplicate(79,$#).

trcfiles(Nod, Files) ->
    case match_files(regExp(trc, Nod), lists:sort(Files), [], []) of
	{[], _} -> exit({?MODULE, no_trc_files});
	{Trc, _} -> Trc
    end.

match_files(_, [], Ok, Nok) -> {Ok, Nok};
match_files(REP, [File|Files], Ok, Nok) ->
    case regexp:match(File, REP) of
	{match, _, _} -> match_files(REP, Files, [File|Ok], Nok);
	nomatch -> match_files(REP, Files, Ok, [File|Nok])
    end.
regExp(Tag, Nod) -> 
    {value, {_, RE}} = lists:keysearch(Tag, 1, ?FILETYPES),
    regexp_parse(Nod++RE).

regexp_parse(RE) ->
    case regexp:parse(RE) of
	{ok, REP} -> REP;
	{error, Err} -> exit({?MODULE, regexp:format_error(Err)})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dout(Form, Args) ->
    case fdo() of 
	{FD, excel} -> ok;
	{FD, _} -> io:fwrite(FD, Form, Args)
    end.
open(File) when list(File) -> open({File, normal});
open({File, Opt}) when Opt==normal; Opt==excel; Opt==wide -> 
    {ok, FD} = file:open(File, [write]),
    put(fd, {FD, Opt}).
close() -> 
    file:close(fd()),
    erase(fd).
fd() ->
    case get(fd) of
	undefined -> standard_io;
	{FD, O} -> FD
    end.
fdo() ->
    case get(fd) of
	undefined -> {standard_io, normal};
	{FD, O} -> {FD, O}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
etsi() -> ets(catch ets:lookup(panScanInfo, tabi)).

ets([{tabi, L1}]) -> ets([], L1);
ets([{tabi, L2},{tabi, L1}]) -> ets(L1, L2);
ets(X) ->       io:fwrite("~p: no ETS info, ~p~n", [?MODULE, X]).

ets(L1, L2) -> prez(do_etsi(L1, L2)).

do_etsi(L1, L2) ->
    do_etsi(do_etsi_filt(L1, []), do_etsi_filt(L2, []), [], [], []).
do_etsi_filt([], O) -> lists:sort(O);
do_etsi_filt([H|T], O) ->
    case lists:member(element(3, H), ?EXCLTABS) of
	true -> do_etsi_filt(T, O);
	false -> do_etsi_filt(T, [etsi_key(H)|O])
    end.
etsi_key({T,I,N,S,M}) -> 
    {list_to_atom(lists:flatten(to_str(T)++to_str(I))),T,I,N,S,M}.

do_etsi([], [], O, B, A) -> {O, B, A};
do_etsi([], [H|T], O, B, A) -> do_etsi([], T, O, B, [diff(a, H)|A]);
do_etsi([H|T], [], O, B, A) -> do_etsi(T, [], O, [diff(b, H)|B], A);
do_etsi([H1|T1], [H2|T2], O, B, A) when element(1, H1) == element(1, H2) ->
    do_etsi(T1, T2, [diff(H2, H1)|O], B, A);
do_etsi(L1 = [H1|T1], [H2|T2], O, B, A) when H1 > H2 ->
    do_etsi(L1, T2, O, B, [diff(a, H2)|A]);
do_etsi([H1|T1], L2 = [H2|T2], O, B, A) when H1 < H2 ->
    do_etsi(T1, L2, O, [diff(b, H1)|B], A).

diff(F, {X,T,I,N,undefined,M}) -> diff(F, {X, T, I, N, 0, M});
diff(F, {X,T,I,N,S,undefined}) -> diff(F, {X, T, I, N, S, 0});
diff({X,T,I,N,undefined,M}, F) -> diff({X, T, I, N, 0, M}, F);
diff({X,T,I,N,S,undefined}, F) -> diff({X, T, I, N, S, 0}, F);
diff(b, {_,T,I,N,S,M}) -> {T, I, N, {-S, 0}, {-M, 0}};
diff(a, {_,T,I,N,S,M}) -> {T, I, N, {S, S}, {M, M}};
diff({_,T,I,N,S2,M2}, {_,T,I,N,S1,M1}) -> {T, I, N, {S2-S1, S2}, {M2-M1, M2}}.

prez({O, B, A}) ->
    D = lists:duplicate(72, $=),
    dout( "ets ===~s~n", [D]), prez(ets, O, B, A, D),
    dout( "dets ==~s~n", [D]), prez(dets, O, B, A, D).
prez(T, O, B, A, D) ->
    dout( ?ETSFORM, [type, id, name, size, diff, mem_Byte, diff]),
    dout( "Change ~s~n", [D]), Tot0 = prez(T, O, 0),
    dout( "Before ~s~n", [D]), Tot1 = prez(T, B, Tot0),
    dout( "After =~s~n", [D]), Tot2 = prez(T, A, Tot1),
    dout( "=======~s~n", [D]), tot(Tot2).
tot(Tot) -> dout( ?ETSFORM, [x, total, x, x, x, x, Tot]).

prez(T, L, Tot) -> prez_c(T, lists:reverse(lists:keysort(5, L)), Tot).
prez_c(T, [], Tot) -> Tot;
prez_c(T, [{T, {I, _}, N, S, M}|R], Tot) -> prez_c(T, [{T, I, N, S, M}|R], Tot);
prez_c(T, [{T, N, N, S, M}|R], Tot) -> prez_c(T, [{T, x, N, S, M}|R], Tot);
prez_c(T, [{T, I, N, {0, S}, {0, M}}|R], Tot) -> prez_c(T, R, Tot);
prez_c(T, [{T, I, N, {DS, S}, {DM, M}}|R], Tot) ->
    dout( ?ETSFORM, [T, I, to_str(N), S, DS, mem(T, M), mem(T, DM)]),
    prez_c(T, R, Tot+mem(T, DM));
prez_c(T, [_|R], Tot) -> prez_c(T, R, Tot).

%%%mem(_, undefined) -> 0;
mem(ets, M) -> M*4;
mem(dets, M) -> M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cpui([Trc1]) ->       panCpu1:go(fdo(), Trc1);
cpui([Trc1, Trc2]) -> panCpu2:go(fdo(), Trc1, Trc2);
cpui(X) ->            io:fwrite("~p: no CPU info, ~p~n", [?MODULE, X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sysi(CpuTime) -> 
    times(catch ets:lookup(panScanInfo, times),  CpuTime),
    sys(catch ets:match(panScanInfo,{sysi,{'$2',{init,'$1'}}})).

times([{times, {T0, T1}}], CpuTime) when number(CpuTime)->
    RunTime = ntdiff(T1, T0),
    dout( "trace ran for ~w s, started ~s~naverage load was ~5.1f%~n", 
	      [RunTime, ntform(T0), (CpuTime/10000)/RunTime]);
times(_, _) -> ok.

sys([]) -> ok;
sys([[Tag, Port]|T]) -> 
    ok = dout( "~p ===~n", [Tag]),
    syso(catch ets:match(panScanInfo,{sysi,{Port,{data,{eol,'$1'}}}})),
    sys(T);
sys(X) -> ok.

syso([]) -> ok;
syso([[E]|T]) ->
    dout( "~s~n", [E]),
    syso(T);
syso(_) -> ok.

to_str(I) when integer(I) -> integer_to_list(I);
to_str(A) when atom(A) -> atom_to_list(A);
to_str(L) when list(L) -> L;
to_str(T) -> io_lib:fwrite("~w", [T]).

ntform({Msec, Sec, Usec} = Now) ->
    {D, T} = calendar:now_to_datetime(Now),
    [Y|L] = tuple_to_list(D)++tuple_to_list(T),
    io_lib:fwrite("~2.2.0w~2.2.0w~2.2.0w-~2.2.0w:~2.2.0w:~2.2.0w", [Y-2000|L]).
ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi)/1000000;
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)/1000000+(So-Si);
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)/1000000+(So-Si)+(MSo-MSi)*1000000.
