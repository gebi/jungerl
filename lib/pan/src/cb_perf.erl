%%%----------------------------------------------------------------------
%%% File    : cb_perf.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : cpu time per pid
%%% Created : 28 Feb 2000 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(cb_perf).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(state, {tab, in = init, gc = init, fd = init, prev}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
requires() -> [{flags, [garbage_collection,procs,running]}, {tps, []}].
doc() -> "populates the cpu time table. "
	     "not useful on its own. run from pan:perf".

%%% obsoleted 
%%%go([Msg, Line, Out, initial, Tab|_]) when atom(Tab) ->
%%%    go([Msg, Line, Out, #state{tab = Tab}]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
go([Msg, Line, Out, initial|_]) ->
    panEts:new(?MODULE),
    go([Msg, Line, Out, #state{tab = ?MODULE}]);
go([end_of_trace, Line, Out, State|_]) ->
    State;
go([Msg, Line, Out, State|_]) ->
    (handle(Msg, State))#state{prev = element(4,Msg)}.

handle(Msg, State = #state{tab = Tab, in = In, prev = Prev}) ->
    case Msg of
	{out, _, 0, Now} ->
	    ets_upd(Tab, {file_driver, in}, 1),
	    State#state{fd = {Now}};
	{out, P, _, Prev} ->			       %early r7b emu behaviour
	    handle({out, P, 0, Prev}, State);
	{in, _, 0, Now} ->
	    do_fd_out(Tab, Now, State);
	{in, P, _, Now} when tuple(State#state.fd) ->  %early r7b emu behaviour
	    handle({in, P, 0, Now}, State);
	{gc_start, {Pid, ID}, Info, Now} ->
	    ets_upd(Tab, {Pid, gc}, 1),
	    State#state{gc = {Pid, Now}};
	{gc_end, {Pid, ID}, Info, Now} ->
	    do_gc_out(Tab, Pid, Now, State);
	{in, {Pid, ID}, CurrF, Now} ->
	    ets_upd(Tab, {Pid, in}, 1),
	    State#state{in = {Pid, Now}};
	{out, {Pid, ID}, CurrF, Now} ->
	    do_out(Tab, Pid, Now, State);
	{exit, {Pid, ID}, Reason, Now} ->
	    case In of
		{Pid,_} -> do_out(Tab, Pid, Now, State);
		_ -> State
	    end;
	_ -> 
	    State
    end.

do_out(Tab, Pid, Now, State = #state{in = In}) ->
    case In of 
	{InPid, InNow} ->
	    Ntd = ntdiff(Now, InNow),
	    ets_upd(Tab, total, Ntd),
	    ets_upd(Tab, {Pid, cpu}, Ntd),
	    State#state{in = no};
	init -> 
	    State;
	Err -> 
	    io:fwrite("~p - out_err~n~p~n", [?MODULE, {Err, Pid, Now, State}]),
	    State
    end.
do_gc_out(Tab, Pid, Now, State = #state{in = In, gc = GC}) ->
    case {In, GC} of 
	{{InPid, _}, {GcPid, GcNow}}  -> 
	    Ntd = ntdiff(Now, GcNow),
	    ets_upd(Tab, {InPid, cpu}, -Ntd),
	    ets_upd(Tab, {GcPid, gcpu}, Ntd),
	    State#state{gc = no};
	{_, {GcPid, GcNow}}  -> 
	    Ntd = ntdiff(Now, GcNow),
	    ets_upd(Tab, total, Ntd),
	    ets_upd(Tab, {Pid, gcpu}, Ntd),
	    State#state{gc = no};
	{_, init} -> 
	    State;
	Err -> 
	    io:fwrite("~p - gc_out_err~n~p~n", [?MODULE,{Err, Pid, Now, State}]),
	    State
    end.
do_fd_out(Tab, Now, State = #state{in = In, gc = GC, fd = FD}) ->
    case {In, GC, FD} of
	{{InPid, _}, {GcPid, _}, {FdNow}} -> 
	    Ntd = ntdiff(Now, FdNow),
	    ets_upd(Tab, total, -Ntd),
	    ets_upd(Tab, {GcPid, gcpu}, -Ntd),
	    ets_upd(Tab, {file_driver, cpu}, Ntd),
	    State#state{fd = no};
	{{InPid, _}, _, {FdNow}} -> 
	    Ntd = ntdiff(Now, FdNow),
	    ets_upd(Tab, total, -Ntd),
	    ets_upd(Tab, {InPid, cpu}, -Ntd),
	    ets_upd(Tab, {file_driver, cpu}, Ntd),
	    State#state{fd = no};
	{_, {GcPid, _}, {FdNow}} -> 
	    Ntd = ntdiff(Now, FdNow),
	    ets_upd(Tab, total, -Ntd),
	    ets_upd(Tab, {GcPid, gcpu}, -Ntd),
	    ets_upd(Tab, {file_driver, cpu}, Ntd),
	    State#state{fd = no};
	{_, _, {FdNow}} -> 
	    Ntd = ntdiff(Now, FdNow),
	    ets_upd(Tab, {file_driver, cpu}, Ntd),
	    State#state{fd = no};
	{_,_,init} -> State;
	Err -> 
	    io:fwrite("~p - fd_out_err~n~p~n", [?MODULE, {Err, Now, State}]),
	    State
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_upd(Tab, Key, Inc) ->
    case catch ets:update_counter(Tab, Key, Inc) of
        {'EXIT', _ } -> ets:insert(Tab, {Key, Inc});
        _ -> ok
    end.

ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)+(So-Si)*1000000;
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000.
