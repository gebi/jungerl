%%%----------------------------------------------------------------------
%%% File    : cb_cpu.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : cpu time per pid
%%% Created : 28 Feb 2000 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(cb_cpu).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).
-export([summary/0,summary/1,summary/2]).

-record(state, {tab, quiet}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
requires() -> [{flags, [garbage_collection,procs,running]}, {tps, []}].
doc() -> "populates the cpu time table. "
	     "not useful on its own. run from pan:perf".

go([Msg, Line, Out, initial, Tab]) ->
    put(cb_cpu_now, initial),
    go([Msg, Line, Out, #state{tab = Tab, quiet = yes}]);
go([Msg, Line, Out, initial]) ->
    put(cb_cpu_now, initial),
    Tab = ets_new(),
    io:fwrite("~p: initializing - created table ~w~n", [?MODULE, Tab]),
    go([Msg, Line, Out, #state{tab = Tab, quiet = no}]);
go([end_of_trace, Line, Out, State|_]) ->
    outro(Line, Out, State);
go([Msg, Line, Out, State|_]) ->
    handle(Msg, State#state.tab),
    State.

%%% "raimo" refers to a bug in the R7B(up to 3) emulator
%%% getting_linked & getting_unlinked can mangle the following in/out msgs
handle(Msg, Tab) ->
    case Msg of
	{T, _, PI, Now} when T == getting_linked; T == getting_unlinked ->%raimo
	    put(cb_cpu_hack, {fix_out, Now, PI});      %raimo
	{in, {PID, ID}, CurrF, Now} ->
	    case get(cb_cpu_hack) of		       %raimo
		{fix_in, PI} ->			       %raimo
		    erase(cb_cpu_hack),		       %raimo
		    handle({in, PI, CurrF, Now}, Tab); %raimo
		_ ->				       %raimo
		    ets_upd(Tab, {PID, in}, 1),
		    doOne(in, Tab, PID, Now)
	    end;				       %raimo
	{out, {PID, ID}, CurrF, Now} ->
	    case get(cb_cpu_hack) of		       %raimo
		{fix_out, Now, PI} ->		       %raimo
		    put(cb_cpu_hack, {fix_in, PI}),    %raimo
		    handle({out, PI, CurrF, Now}, Tab);	%raimo
		_ ->				       %raimo
		    doOne(out, Tab, PID, Now)
	    end;				       %raimo
	{gc_start, {PID, ID}, Info, Now} ->
	    ets_upd(Tab, {PID, gc}, 1),
	    doOne(in, Tab, {PID, gc}, Now);
	{gc_end, {PID, ID}, Info, Now} ->
	    doOne(out, Tab, {PID, gc}, Now);
	{exit,{PID, ID},Reason, Now} ->
	    doOne(out, Tab, PID, Now);
	_ -> ok
    end.

doOne(out, Tab, PID, Now) ->
    case get(cb_cpu_now) of 
	{PID, Now0} -> 
	    erase(cb_cpu_now),
	    ets_upd(Tab, total, ntdiff(Now, Now0)),
	    ets_upd(Tab, {PID, cpu}, ntdiff(Now, Now0));
	initial -> ok;
	Err -> ok %%io:fwrite("~p:out_error: ~p~n   ~p~n    ~p~n", [?MODULE, Line, PID, Err]) 
    end;
doOne(in, Tab, PID, Now) ->
    case get(cb_cpu_now) of 
	undefined -> ok;
	initial -> ok;
	Err -> ok %%io:fwrite("~p:in_error: ~p~n   ~p~n    ~p~n", [?MODULE, Line, PID, Err])
    end,
    put(cb_cpu_now, {PID, Now}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
outro(_, Out, #state{tab = Tab, quiet = Qt}) ->
    case Qt of
	no -> [do_file(Out, E) || E <- summary(Tab)];
	yes -> ok
    end.

do_file(FD, L) when list(L) -> do_file(FD, list_to_tuple(L));
do_file(FD, E) when element(1, E) == pid ->
    do_file_w(FD, E);
do_file(FD, E) when pid(element(1, E)) ->
    do_file_w(FD, setelement(1, E, list_to_atom(pid_to_list(element(1, E)))));
do_file(_, _) -> ok.

do_file_w(FD, E) -> io:fwrite(FD, "~p.~n", [E]).

summary() -> %exported summary function
    summary(?MODULE).
summary(Tab, N) -> %exported summary function
    [[?MODULE, {total,ets_lup(Tab, total)}]|lists:sublist(summary(Tab), N)].
summary(Tab) -> 
    Fun = 
	fun([{P, gc}, T]) -> 
		ets_ins(Tab, {{P, gc_cpu}, T}),
		ets_del(Tab, {P, now}),
		ets_mdl(Tab, {{{P, gc}, '_'},'_'});
	   ([P, T]) -> 	
		ets_ins(Tab, {{P, info}, panScan:proc_tag(P)}) 
	end,
    lists:foreach(Fun,  ets_mch(Tab, {{'$1', cpu}, '$2'})),
    summary(Tab, 2, [info, cpu, in, gc_cpu, gc]).
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
ets_new() -> ets_new(?MODULE).
ets_new(Tab) -> ets_new(Tab, [ordered_set]).
ets_new(Tab, Opts) ->
    catch ets:delete(Tab),
    ets:new(Tab, [named_table,public]++Opts).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	{'EXIT', _} ->[];
	[{Key, R}] -> R;
	R -> R
    end.
ets_ins(Tab, Rec) ->
    catch ets:insert(Tab, Rec).
ets_upd(Tab, Key, Inc) ->
    case catch ets:update_counter(Tab, Key, Inc) of
        {'EXIT', _ } -> ets:insert(Tab, {Key, Inc});
        _ -> ok
    end.
ets_mch(Tab, Pat) ->
    case catch ets:match(Tab, Pat) of
	{'EXIT', _} -> [];
	L -> L
    end.
ets_del(Tab, Key) ->
    catch ets:delete(Tab, Key).
ets_mdl(Tab, Pat) ->
    catch ets:match_delete(Tab, Pat).

ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)+(So-Si)*1000000;
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000.
