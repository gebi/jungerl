%%%----------------------------------------------------------------------
%%% File    : cb_zpu.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : cpu time per pid
%%% Created : 28 Feb 2000 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(cb_zpu).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(state, {tab, quiet}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
requires() -> [{flags, [procs,running]}, {tps, []}].
doc() -> "for each second, prints the 5 processes that uses the most CPU time."
	     "also prints the total CPU time used in that second, and the"
	     "first and last line number in the trace file".

go([Msg, Line, Out, initial]) ->
    put(cb_cpu_now, initial),
    Tab = ets_new(),
    io:fwrite("~p: initializing - created table ~w~n", [?MODULE, Tab]),
    go([Msg, Line, Out, #state{tab = Tab, quiet = no}]);
go([end_of_trace, Line, Out, State|_]) ->
    outro(Line, Out, State);
go([Msg, Line, Out, State|_]) ->
    handle(Msg, State#state.tab, Line),
    State.

%%% "raimo" refers to a bug in the R7B(up to 3) emulator
%%% getting_linked & getting_unlinked can mangle the following in/out msgs
handle(Msg, Tab, Line) ->
    liner(Tab, Msg, Line),
    case Msg of
	{spawn,_,{{_,{application_master,{App}}},_},{_,Sec,_}} ->
	    ets_ins(Tab, {{appl, Sec, App}});
	{T, _, PI, Now} when T == getting_linked; T == getting_unlinked -> %raimo
	    put(cb_cpu_hack, {fix_out, Now, PI});      %raimo
	{in, PI, CurrF, Now} ->
	    case get(cb_cpu_hack) of		       %raimo
		{fix_in, PI0} ->		       %raimo
		    erase(cb_cpu_hack),		       %raimo
		    handle({in, PI0, CurrF, Now}, Tab, Line); %raimo
		_ ->				       %raimo
		    ets_upd(Tab, {PI, in}, 1),
		    doOne(in, Tab, PI, Now, Line)
	    end;				       %raimo
	{out, PI, CurrF, Now} ->
	    case get(cb_cpu_hack) of		       %raimo
		{fix_out, Now, PI0} ->		       %raimo
		    put(cb_cpu_hack, {fix_in, PI0}),   %raimo
		    handle({out, PI0, CurrF, Now}, Tab, Line);%raimo
		_ ->				       %raimo
		    doOne(out, Tab, PI, Now, Line)
	    end;				       %raimo
	{exit, PI, Reason, Now} ->
	    doOne(out, Tab, PI, Now, Line);
	_ -> ok
    end.

doOne(out, Tab, {PID, _} = PI, {_, S, _} = Now, Line) ->
    case get(cb_cpu_now) of 
	{PID, Now0} -> 
	    erase(cb_cpu_now),
	    ets_upd(Tab, total, ntdiff(Now, Now0)),
	    ets_upd(Tab, {total, S}, ntdiff(Now, Now0)),
	    ets_upd(Tab, {PI, cpu}, ntdiff(Now, Now0)),
	    ets_upd(Tab, {PI, S, cpu}, ntdiff(Now, Now0));
	initial -> ok;
	Err -> ok %%io:fwrite("~p:out_error: ~p~n   ~p~n    ~p~n", [?MODULE, Line, PID, Err]) 
    end;
doOne(in, Tab, {PID, _}, Now, Line) ->
    case get(cb_cpu_now) of 
	undefined -> ok;
	initial -> ok;
	Err -> ok %%io:fwrite("~p:in_error: ~p~n   ~p~n    ~p~n", [?MODULE, Line, PID, Err])
    end,
    put(cb_cpu_now, {PID, Now}).

liner(Tab, {_, _,_, {_, Sec, _}}, Line) ->
    case ets_lup(Tab, {sec, Sec}) of
	[] -> ets_ins(Tab, {{sec, Sec}, {Line, 0}});
	{FLine, _} -> ets_ins(Tab, {{sec, Sec}, {FLine, Line}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
outro(_, Out, #state{tab = Tab, quiet = Qt}) ->
    Secs = ets_mch(Tab, {{total, '$1'}, '_'}),
    [S0] = hd(Secs),
    lists:foreach(fun([S]) -> dout(Tab, S, S0) end, Secs).
dout(Tab, N, N0) ->
    [[Tot]] = ets_mch(Tab, {{total, N}, '$1'}),
    {FL, LL} = ets_lup(Tab, {sec, N}),
    case ets_mch(Tab, {{appl, N, '$1'}}) of
	[] -> ok;
	Apps ->
	    io:fwrite("####################~n", []),
	    lists:foreach(fun(A) -> io:fwrite("started ~p~n", A) end, Apps)
    end,
    io:fwrite("####~n~p - ~p ms (~p,~p)~n", [N-N0, trunc(Tot/1000), FL, LL]),
    io:fwrite("~p~n", [head(ets_mch(Tab, {{'$2', N, cpu}, '$1'}), 5, Tot)]).
head(L, N, Tot) ->
    [{trunc(100*I/Tot), P} || [I, P] <- lists_top(L, N)].
lists_top(L, N) ->
    lists:sublist(lists:reverse(lists:sort(L)), N).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_new() -> ets_new(?MODULE).
ets_new(Tab) -> ets_new(Tab, [ordered_set]).
ets_new(Tab, Opts) ->
    catch ets:delete(Tab),
    ets:new(Tab, [named_table,public]++Opts).
ets_ins(Tab, Rec) -> 
    catch ets:insert(Tab, Rec).
ets_upd(Tab, Key, Inc) ->
    case catch ets:update_counter(Tab, Key, Inc) of
        {'EXIT', _ } -> ets:insert(Tab, {Key, Inc});
        _ -> ok
    end.
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	{'EXIT', _} ->[];
	[{Key, R}] -> R;
	R -> R
    end.
ets_mch(Tab, Pat) ->
    case catch ets:match(Tab, Pat) of
	{'EXIT', _} -> [];
	L -> L
    end.

ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)+(So-Si)*1000000;
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000.
