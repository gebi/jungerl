%%%-------------------------------------------------------------------
%%% File    : cb_elwis.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created : 10 Jan 2003 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
%%% Message - {Tag, {Pid, Name}, Data, TimeStamp}
%%%
%%% Tag				  Data 
%%% 'receive',			  Message 
%%% call,			  {M,F,A} 
%%% exit,			  Reason 
%%% return_to,			  {M,F,A} 
%%% spawn,			  Pid2 
%%% link,			  Pid2 
%%% unlink,			  Pid2 
%%% getting_linked,		  Pid2 
%%% in,				  {M,F,A} 
%%% out,			  {M,F,A} 
%%% gc_start,			  Info 
%%% gc_end,			  Info 
%%% send,			  {Pid2, Msg} 
%%% send_to_non_existing_process, {Msg, Pid2} 
%%% return_from,		  {{M,F,A}, ReturnValue}
%%%----------------------------------------------------------------------

-module(cb_elwis).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(state, {}).

requires() -> [{flags, []}, {tps, []}].
doc() -> "stores everything in an ets table".

go([Message, Line, Out, initial|_]) ->
    io:fwrite(Out, "~p: initializing~n", [?MODULE]),
    ets_new(),
    go([Message, Line, Out, initial_state()]);
go([end_of_trace, Line, Out, State|_]) ->
    io:fwrite(Out, "~p: exiting - msgs: ~w~n", [?MODULE, Line-1]);
go([{Tag, {Pid, _}, _, _} = Msg, Line, Out, State|_]) ->
    ets_upd({pid, Pid}),
    ets_upd({tag, Tag}),
    ets_upd({pid_tag, Pid, Tag}),
    ets_ins({{data, Pid, Tag, Msg}}),
    State.

initial_state() ->
    #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_new() -> ets_new(?MODULE).
ets_new(Tab) ->
    catch (Tab ! quit),
    Self = self(),
    spawn(fun() -> register(Tab, self()),
		   ets:new(Tab,[named_table,public,ordered_set]), 
		   Self ! {ets_new, Tab},
		   receive quit -> ok end end),
    receive {ets_new, Tab} -> ok end.
ets_ins(Rec) -> ets_ins(?MODULE, Rec).
ets_ins(Tab, Rec) ->
    catch ets:insert(Tab, Rec).
ets_upd(Key) -> ets_upd(Key, 1).
ets_upd(Key, Inc) -> ets_upd(?MODULE, Key, Inc).
ets_upd(Tab, Key, Inc) ->
    case catch ets:update_counter(Tab, Key, Inc) of
        {'EXIT', _ } -> 
	    ets:insert(Tab, {Key, Inc}), 
	    0;
        Old -> Old
    end.
