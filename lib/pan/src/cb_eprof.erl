%%%----------------------------------------------------------------------
%%% File    : cb_eprof.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 20 Aug 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------
%%%ets:select(cb_eprof,[{{{'_',saalSigPort},stack,'$1'},'_','_'},[{'==',{{saalSigPort,loop,1}},{hd, '$1'}}],['$1']])
-module(cb_eprof).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(state, {tab, quiet}).

doc() -> "populates the prof table. "
	     "not useful on its own. run from pan:prof".
requires() -> [{flags, [garbage_collection,running,procs,return_to,arity]}, 
	       {tps, []}].

go([Msg, Line, Out, initial, Tab|_]) ->
    erase(curr),
    go([Msg, Line, Out, #state{tab = Tab, quiet = yes}]);
go([Msg, Line, Out, initial]) ->
    Tab = ets_new(),
    erase(curr),
    go([Msg, Line, Out, #state{tab = Tab, quiet = no}]);
go([end_of_trace, Line, Out, State|_]) ->
    ok = io:fwrite("~p:~p: end of trace~n", [?MODULE, ?LINE]);
go([Msg, Line, Out, State|_]) ->
    handle(Msg, State#state.tab),
    State.

handle(Msg, Tab) ->
    case Msg of
	{in, PID, CurrF, Now} ->
	    do_stack(in, Tab, PID, CurrF),
	    ets_upd(Tab, {total, PID}, {3, 1}),
	    do_one(in, Tab, PID, Now);
	{out, PID, CurrF, Now} ->
	    do_one(out, Tab, PID, Now);
	{gc_start, PID, Info, Now} ->
	    do_one(out, Tab, PID, Now);
	{gc_end, PID, Info, Now} ->
	    do_one(in, Tab, PID, Now);
	{exit, PID, Reason, Now} ->
	    case get(curr) of
		{Pid, _} -> do_one(out, Tab, PID, Now);
		_ -> ok
	    end,
	    currf(Tab, PID, Now),
	    do_stack(exit, Tab, PID, []);
	{spawn, _, {PID, MFA}, Now} ->
	    do_stack(spawn, Tab, PID, check_mfa(MFA));
	{call, PID, MFA, Now} ->
	    currf(Tab, PID, Now),
	    do_stack(call, Tab, PID, check_mfa(MFA));
	{return_to, PID, MFA, Now} ->
	    currf(Tab, PID, Now),
	    do_stack(return, Tab, PID, check_mfa(MFA));
	_ -> ok
    end.

do_stack(exit, Tab, PID, _) ->
    erase({stack, PID});
do_stack(in, Tab, PID, 0) -> 
    ok;
do_stack(in, Tab, PID, MFA) -> 
    case get({stack, PID}) of
	undefined -> do_stack(spawn, Tab, PID, check_mfa(MFA));
	_ -> ok
    end;
do_stack(spawn, Tab, PID, MFA) ->
    put({stack, PID}, [MFA]);
do_stack(call, Tab, PID, MFA) ->
    case get({stack, PID}) of
	undefined ->
	    do_stack(spawn, Tab, PID, MFA);
	[MFA|_] ->
	    ok;
	Stack -> 
	    case lists:member(MFA, Stack) of
		false ->
		    Stakk = [MFA|Stack],
		    ets_upd(Tab, {PID, func, MFA}, {3, 1}),
		    ets_upd(Tab, {PID, stack, lists:reverse(Stakk)}, {3, 1}),
		    put({stack, PID}, Stakk);
		true ->
		    NS = lists:dropwhile(fun(Mfa) -> MFA =/= Mfa end, Stack),
		    put({stack, PID}, NS)
	    end
    end;
do_stack(return, Tab, PID, MFA) ->
    case get({stack, PID}) of
	undefined ->
	    do_stack(spawn, Tab, PID, MFA);
	Stack ->
	    case lists:member(MFA, Stack) of
		false ->
		    do_stack(spawn, Tab, PID, MFA);
		true ->
		    NS = lists:dropwhile(fun(Mfa) -> MFA =/= Mfa end, Stack),
		    put({stack, PID}, NS)
	    end
    end.

check_mfa({M, F, As}) when list(As) -> {M, F, length(As)};
check_mfa(MFA) -> MFA.

do_one(out, Tab, PID, Now) ->
    case get(curr) of
	undefined -> put(curr, null);
	null -> put(curr, null_null);%%can get scheduled out twice (gc, filedr)
	{Pid, Now0} -> 
	    currf(Tab, PID, Now),
	    put(curr, null),
	    ets_upd(Tab, total, ntdiff(Now, Now0)),
	    ets_upd(Tab, {total, PID}, {2, ntdiff(Now, Now0)});
	Err -> io:fwrite("~p:out error ~p: ~p - ~p~n", [?MODULE, Err, Now, PID])
    end;
do_one(in, Tab, PID, Now) ->
    case get(curr) of
	undefined -> 
	    put(curr, null),
	    do_one(in, Tab, PID, Now);
	null -> 
	    currf(Tab, PID, Now),
	    put(curr, {PID, Now});
	null_null -> put(curr, null);
	Err -> io:fwrite("~p:in error ~p: ~p - ~p~n", [?MODULE, Err, Now, PID])
    end.

currf(Tab, PID, Now) ->
    case get(curr) of
	{PID, _} -> 
	    case get({currf, PID}) of
		undefined -> 
		    io:fwrite("~p:currf error: ~p - ~p~n", [?MODULE, Now, PID]);
		NowC ->
		    Tim = ntdiff(Now, NowC),
		    Stack = get({stack, PID}),
		    ets_upd(Tab, {func, hd(Stack)}, Tim),
		    ets_upd(Tab, {PID, func, hd(Stack)}, {2, Tim}),
		    ets_upd(Tab, {stack, lists:reverse(Stack)}, Tim),
		    ets_upd(Tab, {PID, stack, lists:reverse(Stack)}, {2, Tim})
	    end;
	null -> ok;
	undefined -> ok;
	X -> io:fwrite("~p:curr error: ~p - ~p~n", [?MODULE, X, {PID, Now}])
    end,
    put({currf, PID}, Now).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_upd(Tab, Key, Inc) ->
    case catch ets:update_counter(Tab, Key, Inc) of
        {'EXIT', _ } when tuple(Inc) -> 
	    ets:insert(Tab, {Key, 0, 0}), 
	    ets_upd(Tab, Key, Inc);
	{'EXIT', _ } when integer(Inc) ->
	    ets:insert(Tab, {Key, Inc}),
	    Inc;
        Old -> Old
    end.
ets_new() -> ets_new(?MODULE).
ets_new(Tab) -> ets_new(Tab, [ordered_set]).
ets_new(Tab, Opts) ->
    panEts:server(delete, Tab),
    panEts:server(new, {Tab, Opts}).
ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)+(So-Si)*1000000;
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000.
