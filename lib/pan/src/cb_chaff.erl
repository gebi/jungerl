%%%-------------------------------------------------------------------
%%% File    : cb_chaff.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : 
%%%
%%% Created :  7 May 2002 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(cb_chaff).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(state, {hits = 0,
		reg_chaff = [],
		gen_serv_chaff = [],
		mod_chaff = []}).

requires() -> [{flags, []}, {tps, []}].
doc() -> "prints msgs that does not match process tags from chaff.dat".

go([Message, Line, Out, initial|_]) ->
    State = initial_state(),
    io:fwrite(Out, "~p: initializing - ~w~n", [?MODULE, State]),
    go([Message, Line, Out, State]);
go([end_of_trace, Line, Out, State|_]) ->
    io:fwrite("~w exiting - ~w hits~n", [?MODULE, State#state.hits]);
go([{_, {_, Tag}, _, TS} = Msg, Line, Out, State|_]) ->
    {Cand, Chaff} = 
	case Tag of 
	    {proc_lib,{gen_server,GS}} -> {GS, State#state.gen_serv_chaff};
	    _ when tuple(Tag) -> {element(1, Tag), State#state.mod_chaff};
	    _ when atom(Tag) -> {Tag, State#state.reg_chaff}
	end,
    case lists:member(Cand, Chaff) of
	true -> State;
	false -> 
	    io:fwrite(Out, "~8w ~15s - ~w~n", [Line, ntform(TS), Msg]),
	    State#state{hits = State#state.hits+1}
    end.

initial_state() ->
    case file:consult("chaff.dat") of
	{ok, [RegChaff, ModChaff, GenServChaff]} ->
	    #state{reg_chaff = RegChaff,
		   mod_chaff = ModChaff,
		   gen_serv_chaff = GenServChaff};
	_ -> #state{}
    end.

ntform({Msec, Sec, Usec} = Now) ->
    T = tuple_to_list(element(2,calendar:now_to_datetime(Now)))++[Usec],
    io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w", T).
