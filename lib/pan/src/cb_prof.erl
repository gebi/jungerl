%%%-------------------------------------------------------------------
%%% File    : cb_prof.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created :  2 Dec 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(cb_prof).

-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(state, {tab, currf, in = no, gc = no, fd = no, error, prev}).

doc() -> "populates the prof table. "
	     "not useful on its own. run from pan:prof".
requires() -> [{flags, [garbage_collection,running,procs,return_to,arity]}, 
	       {tps, []}].

go([Msg, Line, Out, initial]) ->
    ets_new(),
    go([Msg, Line, Out, initial, ?MODULE]);
go([Msg, Line, Out, initial, Tab|_]) when atom(Tab) ->
    go([Msg, Line, Out, #state{tab = Tab}]);
go([end_of_trace, Line, Out, State|_]) ->
    State;
go([Msg, Line, Out, State|_]) ->
    (do(Msg, Line, State))#state{prev = element(4, Msg)}.

do(Msg, Line, State) ->
    case catch handle(Msg, State) of
	NState = #state{error = undefined} -> NState;
	NState = #state{error = Err} ->
	    io:fwrite("~p: error on: ~p~n~p~n~p~n", [?MODULE, Line, Msg, Err]),
	    NState#state{error = undefined};
	{'EXIT', R} ->
	    io:fwrite("~p: crash on: ~p~n~p~n~p~n", [?MODULE, Line, Msg, R]),
	    exit(crashed)
    end.

handle(Msg, State = #state{tab = Tab, in = In, prev = Prev}) ->
    case Msg of
	{out, _, 0, Now} ->			       %>late r7 filedriver
	    leave_currf(Now, State#state{fd = yes});
	{out, P, _, Prev} ->			       %early r7b filedriver
	    handle({out, P, 0, Prev}, State);
	{in, _, 0, Now} ->			       %>late r7 filedriver
	    enter_currf(Now, State#state{fd = no});
	{in, P, _, Now} when State#state.fd /= no ->   %early r7b filedriver
	    handle({in, P, 0, Now}, State);
	{gc_start, {Pid, ID}, Info, Now} ->	       %gc
	    leave_currf(Now, State#state{gc = yes});
	{gc_end, {Pid, ID}, Info, Now} ->	       %gc
	    enter_currf(Now, State#state{gc = no});
	{in, {Pid, ID}, CurrF, Now} ->
	    do_stack(in, State, Pid, CurrF),
	    enter_currf(Now, State#state{in = {Pid, Now}});
	{out, {Pid, ID}, CurrF, Now} ->
	    (leave_currf(Now, State))#state{in = no};
	{spawn, _, {Pid, MFA}, Now} ->
	    do_stack(in, State, Pid, check_mfa(MFA));
	{exit,{Pid, ID},Reason, Now} ->	      %2 cases; killed pid is in or out
	    NState = leave_currf(Now, State),
	    do_stack(exit, NState, Pid, []),
	    case NState#state.in of
		{Pid, _} -> NState#state{in = no};
		_ -> NState
	    end;
	{call, {Pid, ID}, MFA, Now} ->
	    NState = leave_currf(Now, State),
	    do_stack(call, NState, Pid, check_mfa(MFA)),
	    enter_currf(Now, NState);
	{return_to, {Pid, Id}, MFA, Now} ->
	    NState = leave_currf(Now, State),
	    do_stack(return, NState, Pid, check_mfa(MFA)),
	    enter_currf(Now, NState);
	_ -> 
	    State
    end.


do_stack(in, State = #state{tab = Tab}, Pid, MFA) -> 
    case ets_lup(Tab, {stack, Pid}) of
	undefined -> ets_ins(Tab, {{stack, Pid}, [MFA]});
	_ -> ok
    end,
    State;
do_stack(exit, State = #state{tab = Tab}, Pid, _) ->
    ets_del(Tab, {stack, Pid}),
    State;
do_stack(call, State = #state{tab = Tab}, Pid, MFA) ->
    case ets_lup(Tab, {stack, Pid}) of
	undefined ->
	    do_stack(in, State, Pid, MFA);
	[MFA|_] ->
	    ok;
	Stack -> 
	    case lists:member(MFA, Stack) of
		false ->
		    Stakk = [MFA|Stack],
		    ets_upd(Tab, {{func, calls}, MFA}),
		    ets_upd(Tab, {{func, calls}, Pid, MFA}),
		    ets_upd(Tab, {{stack, calls}, Pid, lists:reverse(Stakk)}),
		    ets_ins(Tab, {{stack, Pid}, Stakk});
		true ->
		    NS = lists:dropwhile(fun(Mfa) -> MFA =/= Mfa end, Stack),
		    ets_ins(Tab, {{stack, Pid}, NS})
	    end
    end,
    State;
do_stack(return, State = #state{tab = Tab}, Pid, MFA) ->
    case ets_lup(Tab, {stack, Pid}) of
	undefined ->
	    do_stack(in, State, Pid, MFA);
	Stack ->
	    case lists:member(MFA, Stack) of
		false ->
		    do_stack(in, State, Pid, MFA);
		true ->
		    NS = lists:dropwhile(fun(Mfa) -> MFA =/= Mfa end, Stack),
		    ets_ins(Tab, {{stack, Pid}, NS})
	    end
    end,
    State.

check_mfa({M, F, As}) when list(As) -> {M, F, length(As)};
check_mfa(MFA) -> MFA.

enter_currf(Now, State = #state{in=In, gc=Gc, fd=Fd, currf=Currf}) ->
    case {In, Gc, Fd} of
	{{Pid, _}, no, no} -> 
	    case Currf of
		undefined -> State#state{currf = {Pid, Now}};
		X -> State#state{error = {State}}
	    end;
	_ -> State
    end.

leave_currf(Now, State = #state{tab = Tab, in = In, gc = GC, currf = Currf}) ->
    case In of
	no -> 
	    State;
	_ -> 
	    case Currf of
		{Pid, NowIn} -> 
		    Tim = ntdiff(Now, NowIn),
		    Stack = ets_lup(Tab, {stack, Pid}),
		    Rstack = lists:reverse(Stack),
		    Func = hd(Stack),
		    ets_upd(Tab, {total,time}, Tim),
		    ets_upd(Tab, {{pid,time}, Pid}, Tim),
		    ets_upd(Tab, {{func,time}, Func}, Tim),
		    ets_upd(Tab, {{func, time}, Pid, Func}, Tim),
		    ets_upd(Tab, {{stack, time}, Rstack}, Tim),
		    ets_upd(Tab, {{stack, time}, Pid, Rstack}, Tim),
		    State#state{currf = undefined};
		undefined when GC == yes -> State;
		X -> State#state{error = State}
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_upd(Tab, Key) -> ets_upd(Tab, Key, 1).
ets_upd(Tab, Key, Inc) ->
    case catch ets:update_counter(Tab, Key, Inc) of
        {'EXIT', _ } -> ets:insert(Tab, {Key, Inc});
        _ -> ok
    end.
ets_new() -> ets_new(?MODULE).
ets_new(Tab) -> ets_new(Tab, [ordered_set]).
ets_new(Tab, Opts) ->
    catch ets:delete(Tab),
    ets:new(Tab, [named_table,public]++Opts).
ets_ins(Tab, Rec) ->
    catch ets:insert(Tab, Rec).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	[{Key, R}] -> R;
	R -> undefined
    end.
ets_del(Tab, Key) ->
    catch ets:delete(Tab, Key).

ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)+(So-Si)*1000000;
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000.
