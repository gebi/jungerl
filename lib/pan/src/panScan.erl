%%%----------------------------------------------------------------------
%%% File    : panScan.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 27 Feb 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(panScan).
-author('etxmacr@avc386').

-include("pan.hrl").
-include_lib("kernel/include/file.hrl").

-export([file/1, file/2, file/3, file/4, file/5]).
-export([check_file/1]).
-export([proc_tag/1]).
-export([mass/1]).

-record(state, {line = 0, 
		hits = 0,
		send = [],
		filter = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
file(FileName) -> file(FileName, '').
file(FileName, Out) -> file(FileName, Out, '').
file(FileName, Out, Filter) -> file(FileName, Out, Filter, 0, '').
file(FileName, Out, Filter, One) -> file(FileName, Out, Filter, One, One).
file(FileName, Out, Filter, Min, Max) ->
    case check_file(FileName) of
	{ok, File} -> action(File, Out, Filter, Min, Max);
	{error, Reason} -> {error, {Reason, FileName}}
    end.

action(FileName, OutFileName, Filter, Min, Max) ->
    data_init(),
    Out = maybe_open(OutFileName),
    Fun = fun(Mess, Stat) -> do(Mess, Out, Min, Max, Stat) end,
%%%    dbg_action(FileName, Fun, Filter),
    file_action(FileName, Fun, Filter),
    maybe_close(Out).
data_init() ->
    erase(first_ts),
    ets_new(),
    ets_new(panScanInfo, [duplicate_bag]).

-define(CHUNKSIZE, 1024).
file_action(FileName, Fun, Filter) ->
    {ok, FD} = file:open(FileName, [read, raw, binary]),
    Stat = initial_state(Filter),
    do_file_action(make_chunk(FD, <<>>), FD, Fun, Stat),
    file:close(FD).
do_file_action(_, FD, Handler, exit) -> 
    ok;
do_file_action(eof, FD, Handler, Stat) -> 
    Handler(end_of_trace, Stat);
do_file_action({Term, Rest}, FD, Handler, Stat) ->
    do_file_action(make_chunk(FD, Rest), FD, Handler, Handler(Term, Stat)).
make_chunk(FD, eof) -> eof;
make_chunk(FD, <<Op, Size:32, Tal/binary>> = Bin) ->
    case Tal of
	<<Term:Size/binary, Tail/binary>> -> {binary_to_term(Term), Tail};
	_ -> make_chunk(FD, get_more_bytes(FD, Bin))
    end;
make_chunk(FD, Bin) -> make_chunk(FD, get_more_bytes(FD, Bin)).
get_more_bytes(FD, Rest) ->
    case file:read(FD, ?CHUNKSIZE) of
	{ok, Bin} -> <<Rest/binary, Bin/binary>>;
	_ -> eof
    end.


initial_state(Filt) -> #state{filter = filter(Filt)}.

filter({cb, L}) when list(L) -> 
    Fun = fun({M, F, S}, {Ms, Line, Out, Ss}) -> 
		  {Ms, Line, Out, [{M, F, M:F([Ms, Line, Out, S])}|Ss]} 
	  end,
    {cb, [{M, go, initial} || M <- L, atom(M)], Fun};
filter({cb, M}) when atom(M) -> 
    {cb, {M, go, []}, initial};
filter({cb, M, F}) when atom(M), atom(F) -> 
    {cb, {M, F, []}, initial};
filter({cb, M, A}) when atom(M), list(A) -> 
    {cb, {M, go, A}, initial};
filter({cb, M, F, A}) when atom(M), atom(F), list(A) -> 
    {cb, {M, F, A}, initial};
filter(Patt) -> Patt.

do(Mess, Out, Min, _, #state{line = Line} = Stat) when Line < Min -> 
    case mass(Mess) of
	[] -> Stat;
	_ -> Stat#state{line = Line+1}
    end;
do(Mess, Out, _, Max, #state{line = Line} = Stat) when Max < Line -> 
    do(end_of_trace, Out, 0, '', Stat),
    exit;
do(Msg, Out, _, _, Stat = #state{send = {send,From,{To,Data},TSs}, 
				 line = Lin, filter = Filt}) ->
    case mass(Msg) of
	{'receive',{PI,To},Data,TSr} -> 	       %send-recv matched
	    Ms = {send_rcv, From, {{PI, To}, Data}, TSs},
	    do_do(Filt, Ms, Out, Lin, Stat#state{send = [], line = Lin+1});
	{'receive',{To,Pid},Data,TSr} -> 	       %send-recv matched
	    Ms = {send_rcv, From, {{To, Pid}, Data}, TSs},
	    do_do(Filt, Ms, Out, Lin, Stat#state{send = [], line = Lin+1});
	[] -> 					       %handle the buffered send
	    do_do(Filt, Stat#state.send, Out, Lin, Stat#state{send = []});
	Ms -> 					       %handle the buffered send
	    do_do(Filt, Ms, Out, Lin, do_do(Filt, Stat#state.send, 
					    Out, Lin, Stat#state{send = []}))
    end;
do(Mess, Out, _, _, Stat = #state{line = Lin}) ->
    case mass(Mess) of
	{send,_,_,_} = Ms -> Stat#state{send = Ms};
	[] -> Stat;
	Ms -> do_do(Stat#state.filter, Ms, Out, Lin, Stat)
    end.

do_do({cb, {M, F, A}, Internal}, Ms, Out, Line, Stat) ->
    NInt = safe_cb(M, F, A, Ms, Line, Out, Internal),
    Stat#state{line = Line+1, filter = {cb, {M, F, A}, NInt}};
do_do({cb, MFSs, Fun}, Ms, Out, Line, Stat) when list(MFSs), function(Fun) ->
    {_, _, _, NL} = lists:foldl(Fun, {Ms, Line, Out, []}, MFSs),
    Stat#state{line = Line+1, filter = {cb, lists:reverse(NL), Fun}};
do_do(Patt, end_of_trace, Out, Line, Stat) ->
    io:fwrite(Out, "~w - hits: ~w - scanned: ~w~n", 
	      [?MODULE, Stat#state.hits, Line-1]);
do_do(Patt, Ms, Out, Line, Stat) ->
    case grep(Patt, Ms) of
	false -> 
	    Stat#state{line = Line+1};
	true -> 
	    write(Out, Line, Ms),
	    Stat#state{line = Line+1, hits = Stat#state.hits+1}
    end.

safe_cb(M, F, A, Ms, Line, Out, Internal) ->
    case catch M:F([Ms, Line, Out, Internal|A]) of
	{'EXIT', {undef, [{Mm,Ff,Aa}|_]}} -> exit({undef, {Mm,Ff,length(Aa)}});
	{'EXIT', R} -> exit(R);
	Nint -> Nint
    end.

grep('', T) -> true;
grep(P, T) when list(P) ->
    case grp(P, T) of
	[] -> true;
	_ -> false
    end;
grep(P, T) -> grep([P], T).

grp([], _) -> [];
grp(P, []) -> P;
grp(P, T) when tuple(T) -> grp(P--[T], tuple_to_list(T));
grp(P, Rf) when reference(Rf) -> grp(P, list_to_atom(erlang:ref_to_list(Rf)));
grp(P, Pid) when pid(Pid) -> grp(P, list_to_atom(pid_to_list(Pid)));
grp(P, L) when list(L) -> 
    case lists:member(L, P) of
	true -> grp(P--[L], []);
	false -> grp(grp(P, hd(L)), tl(L))
    end;
grp(P, T) -> grp(P--[T], []).

write(no_out, _, _) -> ok;
write(Out, Line, Ms) -> io:fwrite(Out, "~8w ~s - ~w~n", [Line, ts(Ms), Ms]).

ts({_, _, _, {_, _, Us} = Now}) ->
    {_, {D, E, F}} = calendar:now_to_datetime(Now),
    lists:flatten(io_lib:fwrite("~2.2.0w~2.2.0w~2.2.0w.~6.6.0w", [D, E, F, Us]));
ts(_) -> [].

check_file(File) -> 
    case check_files(filename:dirname(File), [filename:basename(File)], []) of
	[] -> {error, no_file};
	[F] -> {ok, F};
	Fs -> {error, {many_files, Fs}}
    end.
	    
check_files(Dir, [], O) -> O;
check_files(Dir, [File|Files], O) ->
    case file:read_file_info(F = filename:join(Dir, File)) of
	{ok, #file_info{type = directory}} -> 
	    {ok, Fs} = file:list_dir(F),
	    check_files(Dir, Files, check_files(F, Fs, O));
	{ok, #file_info{type = regular}} -> 
	    case filename:extension(File) of
		".trc" -> check_files(Dir, Files, [F|O]);
		_ -> check_files(Dir, Files, O)
	    end;
	_ -> check_files(Dir, Files, O)
    end.

maybe_open(quiet) -> no_out;
maybe_open(FileName) ->
    case is_string(FileName) of
	false -> standard_io;
	true -> 
	    {ok, FD} = file:open(FileName, [write]),
	    FD
    end.
maybe_close(no_out) -> ok;
maybe_close(standard_io) -> ok;
maybe_close(FD) -> catch file:close(FD).

is_string([]) -> true;
is_string([H|T]) when integer(H), H >= $ , H =< $~ -> is_string(T);
is_string(_) -> false.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% format is {Tag, PI, Data, Timestamp}
%%% PI is {pid(), Info} 
%%% Info is atom()|tuple()
%%% Timestamp is no_time|now()
%%% Data is;
%%%
%%% 'receive',                    Message
%%% call,                         {M,F,A}
%%% exit,                         Reason
%%% return_to,                    {M,F,A}
%%% spawn,                        {Pid2, {M,F,A}}
%%% register                      registered_name
%%% link,                         Pid2
%%% unlink,                       Pid2
%%% getting_linked,               Pid2
%%% in,                           {M,F,A}
%%% out,                          {M,F,A}
%%% gc_start,                     Info
%%% gc_end,                       Info
%%% send,                         {Pid2, Msg}
%%% send_to_non_existing_process, {Msg, Pid2}
%%% return_from,                  {{M,F,A}, ReturnValue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mass(end_of_trace = T) -> 
    ets_ins(panScanInfo, {times, {get(first_ts), get(last_ts)}}), T;
mass({I, _} = R) when I == tabi; I == traci; I == sysi -> 
    ets_ins(panScanInfo, R), [];
mass({proci, Pid, Info}) ->
    handle_proci(Pid, Info), [];
mass({trace, A, B, C}) ->
    mass({trace_ts, A, B, C, no_time});
mass({trace, A, B, C, D}) ->
    mass({trace_ts, A, B, C, D, no_time});
mass({trace_ts, A, B, C, TS}) ->
    handle_ts(TS),
    handle_procs(B, {A, C}),
    mass(B, A, C, TS);
mass({trace_ts, A, B = send, C, D, TS}) ->
    handle_ts(TS),
    mass(B, A, {D, C}, TS); %% flip the send data
mass({trace_ts, A, B, C, D, TS}) ->
    handle_ts(TS),
    handle_procs(B, {A, C, D}),
    mass(B, A, {C, D}, TS);
mass(X) -> ?LOG(error, {unrec_msg, X}), [].

mass(Tag, Pid1, Data, TS) when pid(Pid1) ->
    mass(Tag, {Pid1, proc_tag(Pid1)}, Data, TS);
mass(Tag, PI, Pid2, TS) when pid(Pid2) ->
    {Tag, PI, {Pid2, proc_tag(Pid2)}, TS};
mass(Tag, PI, {Pid2, Data}, TS) when pid(Pid2) ->
    {Tag, PI, {{Pid2, proc_tag(Pid2)}, Data}, TS};
mass(Tag, PI, Data, TS) ->
    {Tag, PI, Data, TS}.

proc_tag(file_driver) -> file_driver;
proc_tag(Pid) ->
    case ets_lup({Pid, registered_name}) of
	[] -> 
	    case ets_lup({Pid, initial_call}) of
		[] -> unknown;
		IC -> IC
	    end;
	Reg -> Reg
    end.

handle_proci(Pid, [Reg, MFA, Mem]) ->
    ets_ins({{Pid, memory}, Mem}),
    ets_ins({{Pid, initial_call}, MFA}),
    case Reg of 
	[] -> ets_ins({{Pid, registered_name}, MFA});
	_ -> ets_ins({{Pid, registered_name}, Reg})
    end.

handle_ts(TS) ->
    case get(first_ts) of
	undefined -> put(first_ts, TS);
	_ -> ok
    end,
    put(last_ts, TS).

handle_procs(register, {Pid, Reg}) ->
    ets_ins({{Pid, registered_name}, Reg});
handle_procs(spawn, {ParentPid, Pid, {M, F, As}}) ->
    ets_ins({{Pid, parent}, {ParentPid, proc_tag(ParentPid)}}),
    ets_ins({{Pid, initial_call}, {M, F, length(As)}}),
    ets_ins({{Pid, registered_name}, {M, F, length(As)}}),
    case catch mangle_ic({M, F, As}) of
	ok -> ok;
	{'EXIT', _} -> ok;
	Reg -> ets_ins({{Pid, registered_name}, Reg})
    end;
handle_procs(_, _) -> [].
mangle_ic(MFA) ->
    case MFA of
	{proc_lib,init_p,[_,_,M,F,A]} -> 
	    {proc_lib, trans_init(M,F,A)};
	{file,file,[_,FileName,_]} ->
	    {file, {atomize(FileName)}};
	{dets,do_open_file,[Tab,FileName,_,_,_,_,_,Ram,_,_,_]} ->
	    {dets, {Tab}};
	{application_master,start_it,[_,{state,_,ApplD,_,_,_},_,_]} ->
	    {appl_data,App,_,_,_,_,_,_,_} = ApplD,
	    {application_master, {App}};
	{erlang,apply,[Fun,[]]} when function(Fun) -> 
	    funi(Fun);
	_ -> 
	    panOpt:mangle_ic(MFA)
    end.

atomize(FileName) ->
    list_to_atom(hd(lists:reverse(string:tokens(FileName, "/")))).

funi(Fun) ->
    case erlang:fun_info(Fun, module) of
	{_, rpc} ->
	    case erlang:fun_info(Fun, env) of
		{_, [_, _, Pid, A, F, M]} when pid(Pid), list(A) ->
		    {rpc, {call_dummy, node(Pid)}};
		{_, [_, Pid, A, F, M]} ->
		    {rpc, {call, node(Pid)}, {M, F, length(A)}};
		{_, [Pid, A, F, M]} when pid(Pid), list(A) ->
		    {rpc, {cast, node(Pid)}, {M, F, length(A)}};
		X -> %io:fwrite("~p~n", [X]),
		    {rpc}
	    end;
	{_, Mod} -> 
	    case erlang:fun_info(Fun, pid) of
		{_, Pid} when pid(Pid) -> {'fun', {Mod, node(Pid)}};
		{_, X} -> {'fun', {Mod, X}}
	    end
    end.

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,Module,_,_]) ->
    {gen_server,Module};
trans_init(gen,init_it,[gen_server,_,_,_,Module|_]) ->
    {gen_server,Module};
trans_init(gen,init_it,[gen_fsm,_,_,Module,_,_]) ->
    {gen_fsm,Module};
trans_init(gen,init_it,[gen_fsm,_,_,_,Module|_]) ->
    {gen_fsm,Module};
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event};
trans_init(M,F,A) ->
    {M,F,length(A)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ets_new() -> ets_new(?MODULE).
ets_new(Tab) -> ets_new(Tab, [ordered_set]).
ets_new(Tab, Attr) -> 
    panEts:server(delete, Tab),
    panEts:server(new, {Tab, [named_table,public]++Attr}).
ets_ins(Rec) -> ets_ins(?MODULE, Rec).
ets_ins(Tab, Rec) -> 
    catch ets:insert(Tab, Rec).
ets_lup(Key) -> ets_lup(?MODULE, Key).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	{'EXIT', _} ->[];
	[{Key, R}] -> R;
	R -> R
    end.
