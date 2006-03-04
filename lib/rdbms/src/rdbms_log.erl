%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is rdbms-1.2.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File    : rdbms_log.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : disk_log external table type for mnesia
%%%
%%% Created :  9 Jan 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_log).

-export([create_external_table/1,
	 delete_external_table/1,
	 is_ram_only/1,
	 verify_cstruct/1,
	 load_table/5]).
-export([table_info/2]).

%% low-level accessor callbacks.
-export([db_chunk/1,
	 db_erase/2,
	 db_erase_tab/1,
	 db_first/1,
	 db_get/2,
	 db_init_chunk/2,
	 db_last/1,
	 db_next_key/2,
	 db_prev_key/2,
	 db_put/2,
	 db_slot/2,
	 db_update_counter/3,
	 match_erase/2,
	 match_object/2,
	 repair_continuation/2,
	 safe_fixtable/2,
	 select/1,
	 select/2,
	 select_init/3]).

-include("mnesia.hrl").

create_external_table(Cs) ->
    case open_log(Cs#cstruct.name, _Repair = true, _Mode = create) of
	{ok, Log} ->
	    mnesia_monitor:close(Log),
	    ok;
	Other ->
	    erlang:error({Other, erlang:get_stacktrace()})
    end.


delete_external_table(Cs) ->
    Dir = mnesia:system_info(directory),
    LogDir = filename:join(Dir,atom_to_list(?MODULE)),
    true = filelib:ensure_dir(LogDir),
    LogName = filename:join(LogDir, atom_to_list(Cs#cstruct.name) ++ ".rlog"),
    ok = file:delete(LogName).


is_ram_only(_Tab) ->
    false.

verify_cstruct(Cs) ->
    #cstruct{ram_copies = Rc, disc_copies = Dc, disc_only_copies = DOc} = Cs,
    case {Rc,Dc,DOc} of
	{[],[],[]} ->
	    case Cs#cstruct.type of 
		ordered_set -> ok;
		bag -> ok;
		Type -> mnesia:abort({combine_error, Cs#cstruct.name,
				      [{external_copies,
					Cs#cstruct.external_copies},
				       {type, Type}]})
	    end,
	    case Cs#cstruct.frag_properties of
		[] ->
		    Cs;
		[_|_] = Props ->
		    case lists:keysearch(hash_module, 1, Props) of
			{value, {_, rdbms_log_hash}} ->
			    Cs;
			{value, {_, mnesia_frag_hash}} ->
			    mnesia:abort(
			      [{combine_error, Cs#cstruct.name,
				[{external_copies, Cs#cstruct.external_copies},
				 {hash_module, mnesia_frag_hash}]}]);
			{value, _Other} ->
			    %% hope for the best...
			    Cs;
			false ->
			    Cs#cstruct{frag_properties = 
				       [{hash_module, rdbms_log_hash}|
					Cs#cstruct.frag_properties]}
		    end
	    end;
	_ ->
	    X = fun([], _) -> [];
		   ([_|_]=Ns, T) -> [{T, Ns}]
		end,
	    mnesia:abort({combine_error, Cs#cstruct.name,
			  [{external_copies, Cs#cstruct.external_copies}|
			   X(ram_copies,Rc) ++ X(disc_copies,Dc) ++
			   X(disc_only_copies,DOc)]})
    end.

load_table(Tab, _LoadReason, Repair, _Type, OnLoadFun) ->
    ok = OnLoadFun(start),
    {ok, Log} = open_log(Tab, Repair, open),
    fold_log(Log, fun(Data,_Acc) -> {continue,OnLoadFun(Data)} end, []),
    ok = OnLoadFun(done).

open_log(Tab, Repair, _Mode) ->
    Dir = mnesia:system_info(directory),
    LogDir = filename:join(Dir,atom_to_list(?MODULE)),
    NameStr = atom_to_list(Tab) ++ ".rlog",
    LogFName = filename:join(LogDir, NameStr),
    true = filelib:ensure_dir(LogFName),
    LogName = log_name(Tab),
    case mnesia_monitor:open_log([{name, LogName},
				  {file, LogFName},
				  {repair, Repair},
				  {type, halt}]) of
	{ok, _} = GoodRes ->
	    GoodRes;
	{repaired, TheLog, _, _} ->
	    {ok, TheLog};
	Error ->
	    Error
    end.


log_name(Tab) ->
    list_to_atom(atom_to_list(Tab) ++ ".rlog").



table_info(Tab, Item) ->
    {info, Tab, Item}.



fold_log(Log, Fun, InitAcc) ->
    chunk_log(Log, disk_log:chunk(Log, start), Fun, InitAcc).

chunk_log(_Log, eof, _, Acc) ->
    Acc;
chunk_log(_Log, {error, Reason}, _Fun, _Acc) ->
    erlang:error({Reason, erlang:get_stacktrace()});
chunk_log(Log, {Cont, Terms}, Fun, Acc) ->
    case Fun(Terms, Acc) of
	{continue, NewAcc} ->
	    chunk_log(Log, disk_log:chunk(Log, Cont), Fun, NewAcc);
	{stop, NewAcc} ->
	    NewAcc;
	{break, Result, MyAcc} ->
	    {Result, {Log, Fun, MyAcc, Cont}};
	{error, Reason} ->
	    erlang:error({Reason, erlang:get_stacktrace()})
    end.

cont_log({Log, Fun, UserAcc, LogCont}) ->
    chunk_log(Log, disk_log:chunk(Log, LogCont), Fun, UserAcc).


db_get(Tab, Key) ->
    Log = log_name(Tab),
    fold_log(
      Log, fun(Terms, Acc) ->
		   {continue, 
		    lists:foldr(
		      fun(Obj, Acc1) when element(2,Obj) == Key ->
			      [Obj|Acc1];
			 (_Obj, Acc1) ->
			      Acc1
		      end, Acc, Terms)}
	   end, []).
			  


db_put(Tab, Val) ->
    disk_log:log(log_name(Tab), Val).

db_init_chunk(_Tab, _N) ->
    [].

db_chunk(_State) ->
    [].

match_object(Tab, Pat) ->
    select(Tab, [{Pat,[],['$_']}]).

select(Tab, Ms) ->
    Log = log_name(Tab),
    MsC = ets:match_spec_compile(Ms),
    RevResult =
	fold_log(
	  Log, fun(Terms, Acc) ->
		       Matches = ets:match_spec_run(Terms, MsC),
		       {continue, lists:reverse(Matches) ++ Acc}
	       end, []),
    lists:reverse(RevResult).


select_init(Tab, Ms, Limit) ->
    Log = log_name(Tab),
    MsC = ets:match_spec_compile(Ms),
    F = fun(Terms, {Acc, N, Total}) ->
		Matches = ets:match_spec_run(Terms, MsC),
		Found = length(Matches),
		NewN = N + Found,
		if NewN >= Limit ->
			{Return, Keep} = lists:split(Limit-N, Matches),
			Result = 
			    lists:reverse(
			      lists:reverse(Return) ++ Acc),
			{break, Result, {Keep, 0, Total+Found}};
		   true ->
			NewAcc = lists:reverse(Matches) ++ Acc,
			{continue, {NewAcc, NewN, Total + Found}}
		end
	end,
    case fold_log(Log, F, {[],0,0}) of
	{_Res, _Cont} = Intermediate -> Intermediate;
	{[], _, 0} ->
	    '$end_of_table';
	{Result, _, N} when N > 0 ->
	    {lists:reverse(Result), '$end_of_table'}
    end.
    


repair_continuation(Cont, _Ms) ->
    Cont.

select(Cont) ->
    case cont_log(Cont) of
	{_Res, _Cont} = Intermediate -> Intermediate;
	{[], _, 0} ->
	    '$end_of_table';
	{Result, _, _} ->
	    {lists:reverse(Result), '$end_of_table'}
    end.

safe_fixtable(_Tab, _Bool) ->
    true.

db_erase(_Tab, _Key) ->
    ok.

match_erase(_Tab, _Pat) ->
    ok.

db_first(Tab) ->
    Log = log_name(Tab),
    fold_log(Log, fun([First|_], _) ->
			  {stop, element(2, First)}
		  end, '$end_of_table').

db_next_key(Tab, Key) ->
    Log = log_name(Tab),
    F = fun([], false) ->
		{stop, false};
	   (Objs, false) ->
		case lists:dropwhile(
		       fun(Obj) when element(2,Obj) =< Key ->
			       true;
			  (_) ->
			       false
		       end, Objs) of
		    [Next|_] ->
			{stop, {true, element(2,Next)}};
		    [] ->
			{continue, false}
		end
	end,
    case fold_log(Log, F, false) of
	false ->
	    '$end_of_table';
	{true, Next} ->
	    Next
    end.


db_last(_Tab) ->
    ok.

db_prev_key(_Tab, _Key) ->
    ok.

db_slot(_Tab, _Pos) ->
    ok.

db_update_counter(_Tab, _C, _Val) ->
    ok.

db_erase_tab(_Tab) ->
    ok.
