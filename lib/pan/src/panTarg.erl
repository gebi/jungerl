%%%----------------------------------------------------------------------
%%% File    : panTarg.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 23 Mar 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(panTarg).
-author('etxmacr@avc386').

-export([go/1,go/2,go/3,go/4,go/5]).
-export([quit/0]).
-export([flags/0]).

-include("pan.hrl").

-define(DEFPID, no_pid).

-define(CHUNKSIZE, 8192).
-define(EXT, ".trc").

%%%----------------------------------------------------------------------

quit() -> ?MODULE ! quit.

go(Filename) -> go(Filename, ?DEFPROCS).
go(Filename, Procs) -> go(Filename, ?DEFFLAGS).
go(FileName, Procs, Flags) -> go(FileName, Flags, ?DEFTPS).
go(FileName, Procs, Flags, TPs) -> go(FileName, Procs, Flags, TPs, ?DEFPID).
go(FileName, OProcs, OFlags, OTPs, HostPid) ->
    case sanity(FileName) of
	{error, _} = Err -> Err;
	{ok, Dir} -> 
	    {Procs, Flags, TPs} = args(OProcs, OFlags, OTPs),
	    Fun = fun() -> init(HostPid, Flags, TPs, Procs, Dir, FileName) end,
	    {ok, erlang:spawn(Fun), Dir}
    end.

sanity(FileName) ->	    
    case panLib:is_r7() of
	yes ->
	    dbg:stop(),
	    case whereis(?MODULE) of
		P when pid(P) -> {error, {?MODULE, already_started}};
		_ -> 
		    case FileName of
			{ip, _, _} -> {ok, ip};
			_ -> {ok, panLib:get_dir(now())}
		    end
	    end;
	no -> {error, beam_not_r7}
    end.

args(OProcs, OFlags, OTPs) ->
    {procs(OProcs),
     (flags(OFlags)--?MANDFLAGS)++?MANDFLAGS,
     (tps(OTPs)--?MANDTPS)++?MANDTPS}.

flags() -> [{F, flags(F)} || F <- [gc,dbg,perf,prof,all]].
flags('') -> ?DEFFLAGS;
flags(gc) ->   ?GCFLAGS;
flags(dbg) ->  ?DBGFLAGS;
flags(proc) -> ?PROCFLAGS;
flags(perf) -> ?PERFFLAGS;
flags(prof) -> ?PROFFLAGS;
flags(all) ->  ?ALLFLAGS;
flags(Flag) when atom(Flag) -> [Flag];
flags(Flags) when list(Flags) -> Flags.

procs('') -> procs(?DEFPROCS);
procs([]) -> [];
procs(existing) -> [existing];
procs(new) -> [new];
procs(all) -> [all];
procs(Reg) when atom(Reg) -> procs([Reg]);
procs(Tup) when tuple(Tup) -> procs([Tup]);
procs([{pid, I2, I3}|Procs]) when integer(I2), integer(I3) -> 
    procs([c:pid(0, I2, I3)|Procs]);
procs([Pid|Procs]) when pid(Pid) -> [Pid|procs(Procs)];
procs([Reg|Procs]) when atom(Reg) ->
    case whereis(Reg) of
	Pid when pid(Pid) -> [Pid|procs(Procs)];
	undefined -> procs(Procs)
    end;
procs([X|Procs]) ->
    ?LOG(error, {bad_proc, {X}}),
    procs(Procs).

tps(TPs) when list(TPs) ->
    case is_string(TPs) of
	true -> [TPs];
	false -> TPs
    end;
tps('') -> ?DEFTPS;
tps(TP) -> [TP].

is_string([]) -> true;
is_string([H|T]) when integer(H), H >= $ , H =< $~ -> is_string(T);
is_string(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(HostPid, OFlags, TPs, Procs, Dir, File) ->
    erlang:register(?MODULE, self()),
    NTPs = assert_tps(TPs),
    Flags = start_trc(Dir, File, OFlags, NTPs, Procs),
    loop(Dir, HostPid, Flags, NTPs, Procs).

loop(Dir, HostPid, OFlags, TPs, Procs) ->
    receive
	{add, XTPs} ->
	    Ts = assert_tps(XTPs),
	    NTPs = Ts--TPs,
	    do_trace_patterns(NTPs),
	    loop(Dir, HostPid, OFlags, TPs++NTPs, Procs);
	{del, XTPs} ->
	    Ts = assert_tps(XTPs),
	    NTPs = Ts--(Ts--TPs),
	    clear_trace_patterns(NTPs),
	    loop(Dir, HostPid, OFlags, TPs--NTPs, Procs);
	{info, []} ->
	    ?LOG(info, {{flags, OFlags}, {tps, TPs}, {procs, Procs}}),
	    loop(Dir, HostPid, OFlags, TPs, Procs);
	{Port, Info} when port(Port) -> 
	    handle_sysinfo(OFlags, {Port, Info}),
	    loop(Dir, HostPid, OFlags, TPs, Procs);
	{mark, File} ->
	    Flags = stop_trc(OFlags),
	    NFlags = start_trc(Dir, File, Flags, TPs, Procs),
	    loop(Dir, HostPid, NFlags, TPs, Procs);
	quit -> 
	    stop_trc(OFlags),
	    dispose_files(Dir, HostPid),
	    exit({?MODULE, node()})
    end.

start_trc(Dir, File, OFlags, TPs, Procs) ->
    Port = trace_port(Dir, File),
    traci(Port, OFlags, TPs, Procs),
    proci(Port),
    tabi(Port),
    sysI(os:type()),
    Flags = [{tracer, Port}|OFlags],
    Ps = start_tracing(Procs, Flags),
    trace_patterns(TPs),
    ?LOG(info, {{file, File}, {flags, Flags}, {tps, TPs}, {procs, Ps}}),
    erlang:garbage_collect(),
    Flags.

trace_port(ip, IP = {ip, Port, Que}) -> 
    ?LOG(info, {opened_ip_port, Port, Que}),
    (dbg:trace_port(ip, {Port, Que}))();
trace_port(Dir, []) ->
    File = filename:join([Dir, panLib:fname(now())])++?EXT,
    (dbg:trace_port(file, File))();
trace_port(Dir, {wrap, Cnt, Siz}) -> 
    File = {filename:join(Dir, panLib:fname(now())++"-"), wrap, ?EXT, Siz, Cnt},
    (dbg:trace_port(file, File))();
trace_port(Dir, Fname) ->
?LOG(info,{Dir, Fname}),
    File = filename:join([Dir, Fname]),
    (dbg:trace_port(file, File))().

start_tracing(Procs, Flags) ->
    Ps = [{reg(P), catch erlang:trace(P, true, Flags)} || P <- Procs],
    erlang:trace(self(), false, Flags),
    [erlang:trace(P, false, Flags) || P <- processes(), filt(P)],
    Ps.

filt(P) -> filt(reg(P), proc_info(P, initial_call)).
filt(net_kernel, _) -> true;
filt(_, {net_kernel,ticker, _}) -> true;
filt(_, {shell, evaluator, _}) -> true;
filt(_, {shell, server, _}) -> true;
filt(_, {group, server, _}) -> true;
filt(_, {user_drv, server, _}) -> true;
filt(_, {?MODULE, _, _}) -> true;
filt(_, _) -> false.

reg(all) -> all;
reg(new) -> new;
reg(existing) -> existing;
reg(Pid) when pid(Pid) ->
    case proc_info(Pid, registered_name) of
	[] -> Pid;
	Reg -> Reg
    end.

stop_trc([{tracer, Port}|Flags] = OFlags) ->
    clear_trace_patterns(),
    erlang:trace(all, false, OFlags),
    tabi(Port),
    sysI(os:type(),Port),
    erlang:port_close(Port),
    Flags.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear_trace_patterns() ->
    clear_trace_pattern({'_','_','_'}, local),
    clear_trace_pattern({'_','_','_'}, global).
clear_trace_patterns(TPs) ->
    lists:foreach(fun({TP, _, Flag}) -> clear_trace_pattern(TP, Flag) end, TPs).

clear_trace_pattern(TP, Flag) ->
    erlang:trace_pattern(TP, false,[Flag]).


trace_patterns(TPs) ->
    clear_trace_patterns(),
    do_trace_patterns(TPs).
do_trace_patterns(TPs) ->
    lists:foreach(fun(TP) -> tp(TP) end, TPs).

assert_tps(TPs) -> assert_tps(TPs, []).
assert_tps([], O) -> O;
assert_tps([TP|TPs], O) ->
    case assert_tp(TP) of
	[] -> assert_tps(TPs, O);
	Ts when list(Ts) -> assert_tps(TPs++Ts, O);
	T when tuple(T) -> assert_tps(TPs, [T|O])
    end.

-define(g, global).
-define(l, local).
-define(A(X), atom(X)).
-define(L(X), list(X)).
-define(I(X), integer(X)).
assert_tp(TP) ->
    case TP of
	Mpatt when ?L(Mpatt) ->                      many_tp(Mpatt, [], ?g);
	{Mpatt, ?l} when ?L(Mpatt) ->                many_tp(Mpatt, [], ?l);
	{Mpatt, ?g} when ?L(Mpatt) ->                many_tp(Mpatt, [], ?g);
	{Mpatt, MS} when ?L(Mpatt), ?L(MS) ->        many_tp(Mpatt, MS, ?g);
	{Mpatt, MS, ?l} when ?L(Mpatt),?L(MS)->      many_tp(Mpatt, MS, ?l);
	{Mpatt, MS, ?g} when ?L(Mpatt),?L(MS)->      many_tp(Mpatt, MS, ?g);
	M when ?A(M) ->                              {{M,'_','_'}, [], ?g};
	{M} when ?A(M) ->                            {{M,'_','_'}, [], ?g};
	{M,?l} when ?A(M)           ->               {{M,'_','_'}, [], ?l};
	{M,?g} when ?A(M)           ->               {{M,'_','_'}, [], ?g};
	{M,MS} when ?A(M),?L(MS) ->                  {{M,'_','_'}, MS, ?g};
	{M,MS,?l} when ?A(M),?L(MS) ->               {{M,'_','_'}, MS, ?l};
	{M,MS,?g} when ?A(M),?L(MS) ->               {{M,'_','_'}, MS, ?g};
	{M,F} when ?A(M),?A(F) ->                    {{M,  F,'_'}, [], ?g};
	{M,F,?l} when ?A(M),?A(F) ->                 {{M,  F,'_'}, [], ?l};
	{M,F,?g} when ?A(M),?A(F) ->                 {{M,  F,'_'}, [], ?g};
	{M,F,MS} when ?A(M),?A(F),?L(MS) ->          {{M,  F,'_'}, MS, ?g};
	{M,F,MS,?l} when ?A(M),?A(F),?L(MS)->        {{M,  F,'_'}, MS, ?l};
	{M,F,MS,?g} when ?A(M),?A(F),?L(MS)->        {{M,  F,'_'}, MS, ?g};
	{M,F,A} when ?A(M),?A(F) ->                  {{M,  F,  A}, [], ?g};
	{M,F,A,?l} when ?A(M),?A(F)->                {{M,  F,  A}, [], ?l};
	{M,F,A,?g} when ?A(M),?A(F)->                {{M,  F,  A}, [], ?g};
	{M,F,A,MS} when ?A(M),?A(F),?L(MS) ->        {{M,  F,  A}, MS, ?g};
	{M,F,A,MS,?l} when ?A(M),?A(F),?L(MS)->      {{M,  F,  A}, MS, ?l};
	{M,F,A,MS,?g} when ?A(M),?A(F),?L(MS)->      {{M,  F,  A}, MS, ?g};
	X -> ?LOG(error, {bad_tp, X}),[]
    end.

many_tp(Mpatt, MS, Flag) ->
    [{M, '_', '_', MS, Flag} || {M, _} <- code:all_loaded(), mtp(M, Mpatt)].
mtp(M, Mpatt) ->
    regexp:match(atom_to_list(M), Mpatt) /= nomatch.

tp({MFA, MS, Flag}) ->
    NMS = map_ms(MS),
    ?LOG(info, {trace_pattern, {MFA, NMS, [Flag]}}),
    catch erlang:trace_pattern(MFA, NMS, [Flag]).
map_ms(MS) ->
    F = fun(stack) -> {'_',[],[{message,{process_dump}}]};
	   (return) -> {'_',[],[{return_trace}]};
	   (X) -> X
	end,
    lists:map(F, MS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dispose_files(ip, _) -> ok;
dispose_files(Dir, HostPid) ->
    erlang:process_flag(priority, low),
    dispose_files(Dir, panLib:ls(Dir), HostPid).
dispose_files(Dir, [], HostPid) -> [];
dispose_files(Dir, [File|Files], HostPid) ->
    do_file(Dir, File, HostPid),
    dispose_files(Dir, Files, HostPid).
do_file(Dir, File, HostPid) ->
    {ok, FD} = file:open(filename:join(Dir, File), [read, raw, binary]),
    do_file(file:read(FD, ?CHUNKSIZE), Dir, File, FD, HostPid).

do_file(eof, Dir, File, FD, HostPid) -> 
    HostPid ! {?MODULE, node(), File, eof},
    handle_eof(FD, filename:join(Dir, File));
do_file({ok, Bin}, Dir, File, FD, HostPid) ->
    HostPid ! {?MODULE, node(), File, {chunk, Bin}},
    do_file(file:read(FD, ?CHUNKSIZE), Dir, File, FD, HostPid);
do_file({error, Reason}, Dir, File, FD, HostPid) ->
    HostPid ! {?MODULE, node(), {error, Reason}, File}.

handle_eof(FD, File) ->
    file:close(FD),
    case file:delete(File) of
	ok -> ok;
	{error, R} -> ?LOG(error, {couldnt_delete, R, File})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
traci(Port, Flags, TPs, Procs) ->
    Traci = {{flags, Flags}, {tps, TPs}, {procs, Procs}, {version, c_dir()}},
    send2port(Port, {traci, Traci}).

c_dir() -> hd([c_dir(L) || {options, L} <- ?MODULE:module_info(compile)]).
c_dir([{outdir, D}|_]) -> D;
c_dir([_|T]) -> c_dir(T);
c_dir([]) -> no_info.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proci(Port) -> 
    lists:foreach(fun(P) -> send_proc_info(P, Port) end, processes()).

send_proc_info(P, Port) ->
    case proc_info(P) of
	[_,_,undefined] -> ok;
	Info -> send2port(Port, {proci, P, Info})
    end.

proc_info(P) -> 
    [proc_info(P, T) || T <- [registered_name, initial_call, memory]].

proc_info(P, Tag) ->
    case process_info(P, Tag) of
	{_, {proc_lib,init_p,5}} -> proc_lib:translate_initial_call(P);
	{_, X} -> X;
	_ -> []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tabi(Port) ->
    L = [get_etsi(T) || T <- ets:all()]++[get_detsi(T) || T <- dets_all()],
    TabI = [E || E <- L, integer(element(4, E))],
    send2port(Port, {tabi, TabI}).
get_etsi(T) ->
    {ets, T, ets:info(T, name), ets:info(T, size), ets:info(T, memory)}.
dets_all() ->
    case whereis(dets) of
	undefined -> [];
	_ -> dets:all()
    end.
get_detsi(T) ->
    catch {dets, T, T, dets:info(T, size), dets:info(T, file_size)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_sysinfo([{tracer, Port}|_], SysI) ->
    handle_sysinfo(Port, SysI);
handle_sysinfo(Port, SysI) when port(Port) ->
    send2port(Port, {sysi, SysI}).

sysI({unix,sunos},Port) ->
    {P, _} = beam_size(),
    handle_sysI(Port, P);
sysI(_,_) -> ok.

handle_sysI(OutPort, InPort) when port(InPort) ->
    receive
	{InPort, {exit_status, _}} -> ok;
	{InPort, _} = Info ->
	    handle_sysinfo(OutPort, Info),
	    handle_sysI(OutPort, InPort)
    after 1000 ->
	    ok
    end.

sysI({unix,sunos}) ->
    mpstat(),
    beam_size();
sysI(_) -> ok.

mpstat() ->
    C = "mpstat 2 2",
    self() ! {open_port({spawn, C}, [{line, 132}]), {init, mpstat}}.

beam_size() ->
    C = "ps -o \"vsz rss pmem pcpu\" -p "++os:getpid()++" | grep -v VSZ",
    self() ! {open_port({spawn, C}, [{line, 132}, exit_status]), {init, ps}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send2port(Port, Bin) when port(Port), binary(Bin) ->
    erlang:port_command(Port, Bin);
send2port(Port, Bin) when binary(Bin) ->
    ?LOG(error, {bad_port, {Port}});
send2port(Port, Term) ->
    send2port(Port, term_to_binary(Term)).

