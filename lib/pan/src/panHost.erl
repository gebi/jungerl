%%%----------------------------------------------------------------------
%%% File    : panHost.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created :  7 Mar 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------
%%%pan:start(ip,all,all,call,{erlang,now,[{'_',[],[{message,{process_dump}}]}]}).
%%%pan:start(ip,all,all,call,{hsnNccI,send_trh_estabPathRes_msg,[{['_','_',2],[],[{message,{process_dump}}]}]}).

-module(panHost).
-author('etxmacr@avc386').

-include("pan.hrl").

-define(FORM, "~s~n~s - ~p - ~p~n~p~n").
-define(DEL, lists:duplicate(77, $=)).

-export([start/0,start/1,start/2,start/3,start/4,start/5]).
-export([mark/0]).
-export([stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> start('').
start(TC) -> start(TC, '').
start(TC, Nods) -> start(TC, Nods, '').
start(TC, Nods, Procs) -> start(TC, Nods, Procs, '').
start(TC, Nods, Procs, Flags) -> start(TC, Nods, Procs, Flags, '').
start(OTC, ONods, OProcs, OFlags, OTPs) ->
    do_start(args(OTC, ONods), OProcs, OFlags, OTPs).

do_start({TC, Nods}, Procs, Flags, TPs) ->
    case whereis(?MODULE) of
	undefined -> 
	    S = self(),
	    Ch = spawn_link(fun() -> init(S, TC, Nods, Procs, Flags, TPs) end),
	    receive {Ch, started} -> ok end,
	    erlang:unlink(Ch);
	_ -> {?MODULE, already_running}
    end.

stop() -> ?MODULE ! stop.
mark() -> ?MODULE ! mark.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
args(OTC, ONods) -> 
    {tc(OTC), nods(ONods)}.

tc('') -> tc(?DEFTC);
tc(ip) -> tc({ip, ''});
tc({ip, CallBack}) when atom(CallBack) -> {ip, {CallBack, ?DEFIPPORT}};
tc({wrap, ''}) -> tc({wrap, ?DEFTC});
tc({wrap, TC}) -> tc({wrap, TC, ?DEFWRAPCNT});
tc({wrap, TC, Cnt}) -> tc({wrap, TC, Cnt, ?DEFWRAPSIZE});
tc({wrap, TC, Cnt, Siz} = X) when integer(Cnt), integer(Siz), atom(TC) -> X;
tc(Tc) when atom(Tc) -> tc({Tc, 0});
tc({Tc, Secs}) when atom(Tc), integer(Secs) -> {timed, Tc, Secs};
tc(Tc) -> exit({bad_tc, Tc}).

nods('') -> nods(panOpt:defnodes());
nods(all) -> nodes();
nods(Node) when atom(Node) -> nods([Node]);
nods(Ns) -> 
    case [N || N <- Ns, nods_filt(N)] of
	[] -> exit({?MODULE, no_nodes});
	O -> O
    end.
nods_filt(N) -> net_adm:ping(N) == pong.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(ParentPid, TC, Nodes, Procs, Flags, TPs) ->
    erlang:process_flag(trap_exit, true),
    ?LOG(dbg, {exiting, catch action(ParentPid, TC, Nodes, Procs, Flags, TPs)}),
    exit(ok).

action(ParentPid, TC, Nodes, Procs, Flags, TPs) ->
    register(?MODULE, self()),
    load_on_target(Nodes, ?MODULES),
    {OutDir, OutFile} = outdir(TC),
    Args = [OutFile, Procs, Flags, TPs, self()],
    ?LOG(dbg, {starting_on, {Nodes, Args}}),
    Pids = assert_started([rpc:call(N, panTarg, go, Args) || N <- Nodes]),
    ParentPid ! {self(), started},
    maybe_start_client(TC, Nodes),
    loop(Pids, OutDir, [], init_timer(TC)),
    dbg:stop().

load_on_target(Nodes, []) -> ok;
load_on_target(Nodes, [Mod|Mods]) ->
    {Mod, Bin, Fname} = code:get_object_code(Mod),
    rpc:eval_everywhere(Nodes, code, load_binary, [Mod, Fname, Bin]),
    load_on_target(Nodes, Mods).

loop([], _, _, _) -> ?LOG(info, 'all target procs done - exiting');
loop(Left, OutDir, FDs, Timer) ->
    receive
	{panDbg, Term} -> 
	    [P ! Term || P <- Left],
	    loop(Left, OutDir, FDs, reset_timer(Timer));
	{panTarg, Node, File, FileData} ->
	    NewFDs = handle_chunk(Node, File, FileData, FDs, OutDir),
	    loop(Left, OutDir, NewFDs, Timer);
	{'EXIT', Pid, R} ->
	    ?LOG(info, {received_exit, node(Pid), R}),
	    loop(Left--[Pid], OutDir, FDs, reset_timer(Timer));
	mark ->
	    [P ! {mark, []} || P <- Left],
	    case Timer#timer.state of
		marked -> kill_targ_procs(Left);
		init -> loop(Left, OutDir, FDs, set_timer(Timer))
	    end;
	kill -> 
	    kill_targ_procs(Left);
	stop ->
	    [P ! quit || P <- Left],
	    loop(Left, OutDir, FDs, Timer);
	X ->
	    ?LOG(error, X),
	    loop(Left, OutDir, FDs, reset_timer(Timer))
    after 
	Timer#timer.timeout ->
	    stop(),
	    loop(Left, OutDir, FDs, Timer)
    end.

outdir({ip, {_, Port}}) -> {no_dir, {ip, Port, ?DEFIPQUE}};
outdir({wrap, TC, Cnt, Siz}) -> {get_outdir(TC), {wrap, Cnt, Siz}};
outdir({timed, TC, _}) -> {get_outdir(TC), []}.

get_outdir(TestCaseName) ->
    FileName = 
	case TestCaseName of
	    ?DEFTC -> "pan-"++panLib:timestamp();
	    _ -> atom_to_list(TestCaseName)
	end,
    OutDir = filename:join([?TMPDIR, "pan", FileName]),
    panLib:delete_dir(OutDir),
    panLib:verify_dir(OutDir),
    OutDir.

init_timer({timed, _, Sec}) when integer(Sec), Sec > 0 -> 
    N = now(),
    #timer{state=init, start=N, stop=ntsum(N, {0, Sec, 0}), timeout=Sec*1000};
init_timer(_) -> 
    #timer{state=init, start=now(), stop=0, timeout=infinity}.

set_timer(#timer{start=Start}) ->
    T = ntdiff(now(), Start),			       %millisecs
    Stop = ntsum(Start, {0, 0, T*1000}),
    ?LOG(info, {mark_set, {secs, round(T/1000)}}),
    #timer{state=marked, start=Start, stop=Stop, timeout=T}.
		    
reset_timer(#timer{timeout=infinity} = T) -> T;
reset_timer(#timer{stop=Stop} = T) -> T#timer{timeout=ntdiff(Stop, now())}.

maybe_start_client({ip, {CB, Port}}, Nodes) ->
    dbg:stop(),
    ?WAIT(3),
    ets_new(panScan),
    start_client(Port, CB, Nodes);
maybe_start_client(_, _) -> ok.

start_client(_, _, []) -> ok;
start_client(Port, CB, [Node|Nodes]) ->
    IpFun = fun(Msg, State) -> ip_handler_wrap(Node, Msg, State) end,
    [_, Host] = string:tokens(atom_to_list(Node), "@"),
    dbg:trace_client(ip, {Host, Port}, {IpFun, {CB}}),
    start_client(Port, CB, Nodes).

ip_handler_wrap(Node, Msg, State) ->
    case catch ip_handler(Node, Msg, State) of
	{'EXIT', R} -> 
	    io:fwrite("crash in ip_handler~n~p~n", [{R, Node, Msg, State}]),
	    State;
	NState -> NState
    end.
ip_handler(Node, end_of_trace, State) -> 
    io:fwrite("ip_handler on ~p exiting~n", [Node]);
ip_handler(Node, {drop, N}, State) ->
    io:fwrite("ip_handler for ~p dropped ~p~n", [Node, N]),
    State;
ip_handler(Node, Msg, {CB}) ->
    io:fwrite("ip_handler on ~p starting - ~p~n", [Node, CB]), 
    case CB of
	'' -> ip_handler(Node, Msg, {'', '', 1});
	_ -> ip_handler(Node, Msg, {CB, initial, 1})
    end;
ip_handler(Node, Msg, {'', '', Line}) ->
    case panScan:mass(Msg) of
	{call, P, {MFA, Bin}, TS} when binary(Bin) -> 
	    BL = binary_to_list(Bin),
	    io:fwrite(?FORM++"~s~n", [?DEL, ntform(TS), Node, P, MFA, BL]);
	{call, P, MFA, TS} -> 
	    io:fwrite(?FORM, [?DEL, ntform(TS), Node, P, MFA]);
	{Tag, P, Info, TS} -> 
	    io:fwrite(?FORM, [?DEL, ntform(TS), Node, P, {Tag, Info}]);
	[] -> ok;
	Huh ->
	    io:fwrite("~p: huh? - ~p", [?MODULE, Huh])
    end,
    {'','',Line+1};
ip_handler(Node, Msg, {CB, CBstate, Line}) ->
    case panScan:mass(Msg) of
	[] -> {CB, CBstate, Line};
	M -> {CB, CB:go([M, Line, standard_io, CBstate]), Line+1}
    end.

handle_chunk(Node, File, eof, FDs, OutDir) ->
    ?LOG(info, {created, Node, filename:join([OutDir, Node, File])}),
    {value, {_, _, FD}} = lists:keysearch(Node, 1, FDs),
    file:close(FD),
    lists:keydelete(Node, 1, FDs);
handle_chunk(Node, File, {chunk, Bin}, FDs, OutDir) ->
    case lists:keysearch(Node, 1, FDs) of
	{value, {Node, _, FD}} ->
	    file:write(FD, Bin),
	    FDs;
	false ->
	    Dir = panLib:verify_dir(filename:join([OutDir, Node])),
	    FileName = filename:join([Dir, File]),
	    ?LOG(dbg, {opened, Node, FileName}),
	    {ok, FD} = file:open(FileName, [write, raw, binary]),
	    file:write(FD, Bin),
	    [{Node, FileName, FD}|FDs]
    end.

assert_started([{ok, P, Dir}|Ress]) ->
    case catch erlang:link(P) of
	true -> [P|assert_started(Ress)];
	Err -> 
	    ?LOG(error, {couldnt_link, {Err}}),
	    assert_started(Ress)
    end;
assert_started([Err|Ress]) ->
    ?LOG(error, {not_started, {Err}}),
    assert_started(Ress);
assert_started([]) -> [].


kill_targ_procs(Ps) ->
    lists:foreach(fun(P) -> erlang:exit(P, killed) end).

ntdiff(T0, T) when T > T0 -> infinity;
ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    round((USo-USi)/1000);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    round(((USo-USi)+(So-Si)*1000000)/1000);
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    round(((USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000)/1000).

ntsum({MS1, S1, US1}, {MS2, S2, US2}) -> ntsum({MS1+MS2, S1+S2, US1+US2}).
ntsum({MS, S, US}) when US > 999999 -> ntsum({MS, S+1, US-1000000});
ntsum({MS, S, US}) when  S > 999999 -> ntsum({MS+1, S-1000000, US});
ntsum(N) -> N.

ntform({Msec, Sec, Usec} = Now) ->
    T = tuple_to_list(element(2,calendar:now_to_datetime(Now)))++[Usec],
    lists:flatten(io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w", T)).

ets_new(Tab) -> 
    catch ets:delete(Tab),
    ets:new(Tab, [named_table,ordered_set,public]).
