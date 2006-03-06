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
%%% File:	rdbms.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Relational constraints checking for mnesia databases
%%% 
%%% Modules used : mnesia, mnesia_schema, lists
%%% 
%%%----------------------------------------------------------------------

-module(rdbms).
-vsn('1.2').
-date('99-12-18').
-author('ulf.wiger@ericsson.com').


%%%----------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%%----------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%%----------------------------------------------------------------------

%% Substitute for mnesia:transaction/1.
-export([activity/1]).
-export([begin_activity/4, end_activity/4]).


-export([load_schema/1, load_schema/2]).

-export([create_table/2,
	 do_create_table/2,
	 delete_table/1,
	 do_delete_table/1]).

-export([make_simple_form/1]).

%%% Table metadata
-export([
%%% 	 attributes/1,			% (Table)
%%% 	 all_attributes/1,		% (Table)
%%% 	 attribute/2,			% (Position, Table)
%%% 	 attribute_value/3,		% (Table, AttrName, Object)
%%% 	 position/2,			% (Attribute, Table)
%%% 	 default_record/1,		% (Table)
%%% 	 verify_write/2,		% (Table, Object)
%%% 	 verify_delete/2,		% (Table, Key)
%%% 	 verify_delete_object/2,	% (Table, Object)
	 register_commit_action/1,	% (function/0)
	 register_rollback_action/1]).	% (function/0)

-export([null_value/0]).
-export([mk_oid/2]).

%%% Update - Mnesia access module callbacks
-export([lock/4,
	 write/5,
	 delete/5,
	 delete_object/5,
	 read/5,
	 select/5,
	 match_object/5,
	 all_keys/4,
	 index_match_object/6,
	 index_read/6,
	 table_info/4,
	 first/3,
	 last/3,
	 next/4,
	 prev/4,
	 foldl/6,
	 foldr/6,
	 select/5,
	 select/6,
	 select_cont/3]).
-export([onload_fun/2]).

%% Used by other rdbms modules (includes VMod)
-export([read/6,
	 do_select/6]).

-export([fetch_verification_module/0,
	 default_verification_module/0]). % for remote procedure calls


-export([patch_mnesia/0]).

%%% extra retrieval functions
%%%-export([fetch_objects/3]).	% (Tab, SelectAttr, SelectKey)

%%%----------------------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%%----------------------------------------------------------------------

-include("rdbms.hrl").
-include_lib("mnesia/src/mnesia.hrl").

-import(mnesia, [abort/1]).
-import(lists, [keysearch/3, foreach/2, foldl/3, foldr/3]).

-define(KEYPOS, 2).

-define(vmod, fetch_verification_module()).
-define(JIT_MODULE, rdbms_verify_jit).

-record(rdbms_activity, {is_schema_transaction = false,
			 verification_module = {rdbms_verify, vrec(false)},
			 refs_logs = [],
			 funs = []}).


%%%----------------------------------------------------------------------
%%% #3.    CODE
%%%----------------------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%%----------------------------------------------------------------------


patch_mnesia() ->
    application:load(mnesia),
    {ok,Ms} = application:get_key(mnesia, modules),
    ToPatch = Ms -- [mnesia, mnesia_controller, mnesia_frag,
		     mnesia_lib, mnesia_loader, mnesia_log,
		     mnesia_schema, mnesia_tm],
    OrigDir = filename:join(code:lib_dir(mnesia), "ebin"),
    lists:foreach(
      fun(M) ->
	      F = filename:join(
		    OrigDir,atom_to_list(M) ++ code:objfile_extension()),
	      {ok,{_M,[{abstract_code,{raw_abstract_v1,Forms}}]}} = 
		  beam_lib:chunks(F, [abstract_code]),
	      [_|TailF] = Forms,
	      io:format("Transforming ~p ... ", [M]),
	      NewTailF = transform_mod(TailF),
	      {ok, Module, Bin} = compile:forms(NewTailF, []),
	      io:format("ok.~n", []),
	      case code:load_binary(Module, foo, Bin) of
		  {module, Module} ->
		      ok;
		  Error ->
		      erlang:error({Error,Module})
	      end
      end, ToPatch).

transform_mod(Fs) ->
    lists:map(fun({attribute,L,record,{cstruct,Flds}}) ->
		      {attribute,L,record,{cstruct, insert_attr(Flds)}};
		 (X) -> X
	      end, Fs).

insert_attr([{record_field,L,{atom,_,load_order},_} = H|T]) ->
    [{record_field,L,{atom,L,external_copies}, {nil,L}},H|T];
insert_attr([H|T]) ->
    [H|insert_attr(T)];
insert_attr([]) ->
    [].


%%%----------------------------------------------------------------------
%%% -type activity(Fun : function())->
%%%     ResultFromFun.
%%% Input: Function object for Mnesia transaction
%%% Output: Result from 
%%% Exceptions: EXIT if transaction aborted
%%% Description: This is a wrapper around mnesia:activity/1.
%%%    It starts a mnesia activity with this module as the activity
%%%    module. This enables all RDBMS integrity checks.
%%%----------------------------------------------------------------------


begin_activity(_Type, Factor, Tid, Ts) ->
    {?MODULE, Tid, Ts} = get(mnesia_activity_state),  % assertion
    if Factor > 1 ->
	    %% transaction restarted
	    #rdbms_activity{refs_logs = Logs} = A =
		get_activity_state(),
	    case Logs of 
		[] ->
		    ok;
		[_|_] ->
		    foreach(fun(Log) -> ets:delete(Log) end, Logs),
		    put_activity_state(
		      A#rdbms_activity{refs_logs = []})
	    end;
       Factor == 1 ->
	    if Ts#tidstore.level == 1 ->
		    undefined = get_activity_state(),
		    {IsSchemaTrans, VMod} = verification_module(),
		    put_activity_state(
		      #rdbms_activity{is_schema_transaction = IsSchemaTrans,
				      verification_module = VMod});
	       Ts#tidstore.level > 1 ->
		    #rdbms_activity{refs_logs = Logs} = A =
			get_activity_state(),
		    case Logs of
			[] ->
			    ok;
			[PrevLog|_] ->
			    NewLog = new_refs_log(),
			    ets:insert(NewLog, ets:tab2list(PrevLog)),
			    put_activity_state(
			      A#rdbms_activity{refs_logs = [NewLog|Logs]})
		    end
	    end
    end.

%%% TODO: mnesia_schema:schema_transaction() doesn't use
%%% mnesia:activity(), so the default access module isn't used.
%%%
end_activity(Result, Type, _Tid, Ts) ->
    #rdbms_activity{is_schema_transaction = IsSchemaTrans,
		    refs_logs = Logs,
		    funs = Funs} = A = 
	get_activity_state(),

    case Ts#tidstore.level of
	1 ->
	    case Logs of
		[] -> ok;
		[RefsLog] ->
		    %% there should never be more than at most one level 
		    %% of refs_log here. Otherwise, something's gone wrong.
		    ets:delete(RefsLog)
	    end,
	    case Result of
		{atomic, _} ->
		    foreach(fun({F, _Level, commit}) ->
				    catch_run(F, commit);
			       (_) ->
				    ok
			    end, lists:reverse(Funs)),
		    if Type == read_only ->
			    case erlang:module_loaded(?JIT_MODULE) of
				false ->
				    io:format("generating (read-only)~n",
					      []),
				    GenRes =
					(catch rdbms_codegen:regenerate()),
				    io:format("GenRes = ~p~n", [GenRes]);
				true ->
				    ok
			    end;
		       IsSchemaTrans; Type == asym_trans ->
			    case ets:select_count(
				   Ts#tidstore.store,
				   [{'$1',[{'==',{element,1,'$1'},'op'}],
				     [true]}]) of
				0 ->
				    io:format("no need to regenerate~n", []);
				N when N > 0 ->
				    io:format("TidStore = ~n~p~n",
					      [ets:tab2list(
						 Ts#tidstore.store)]),
				    io:format(
				      "Schema updated - regenerate!~n", []),
				    GenRes =
					(catch rdbms_codegen:regenerate()),
				    io:format("GenRes = ~p~n", [GenRes])
			    end;
		       true ->
			    ok
		    end;
		{aborted, _} ->
		    foreach(fun({F, _Level, rollback}) ->
				    catch_run(F, rollback);
			       (_) ->
				    ok
			    end, lists:reverse(Funs))
	    end,
	    erase_activity_state();
	L when L > 1 ->
	    case Result of 
		{aborted, _} ->
		    NewFuns = [{F,Level,FType} ||
				  {F,Level,FType} <- Funs,
				  Level =/= L],
		    put_activity_state(A#rdbms_activity{funs = NewFuns});
		{atomic,_} ->
		    case Logs of
			[ThisLog,PrevLog|Rest] ->
			    Refs = ets:tab2list(ThisLog),
			    ets:delete(ThisLog),
			    ets:insert(PrevLog, Refs),
			    put_activity_state(
			      A#rdbms_activity{refs_logs = [PrevLog|Rest]});
			[_SingleLog] ->
			    ok;
			[] ->
			    ok
		    end
	    end
    end.



activity(Fun) ->
    mnesia:activity(transaction, Fun, [], ?MODULE).


create_table(Name, Opts) ->
    mnesia_schema:schema_transaction(
      fun() ->
	      do_create_table(Name, Opts)
      end).

do_create_table(Name, Opts) ->
    case keysearch(rdbms, 1, Opts) of
	{value, {_, Props}} ->
	    Options = lists:keydelete(rdbms,1,Opts),
	    Cs = mnesia_schema:list2cs([{name,Name}|Options]),
	    mnesia_schema:do_create_table(Cs),
	    io:format("created ~p~n" , [Name]),
	    {Indexes, Props1} =
		case keysearch(indexes, 1, Props) of
		    {value, {_, I}} ->
			{I, lists:keydelete(indexes, 1, Props)};
		    false ->
			{[], Props}
		end,
	    lists:foreach(
	      fun({K,V}) -> 
		      rdbms_props:do_set_property(Name,K,V)
	      end, Props1),
%%%	    rdbms_props:do_add_properties(Props1, Name),
	    rdbms_index:do_add_indexes(Name, Indexes);
	false ->
	    Cs = mnesia_schema:list2cs([{name,Name}|Opts]),
	    mnesia_schema:do_create_table(Cs)
    end.

delete_table(Name) ->      
    mnesia_schema:schema_transaction(
      fun() ->
	      do_delete_table(Name)
      end).

do_delete_table(Name) ->
    mnesia_schema:do_delete_table(Name),
    rdbms_groups:do_drop_membership(Name),
    rdbms_props:do_drop_references(Name),
    case rdbms_props:indexes(Name) of
	[_|_] = Indexes ->
	    rdbms_index:do_delete_indexes(Indexes);
	[] ->
	    ok
    end.


load_schema(File) ->
    load_schema(File, erl_eval:new_bindings()).

load_schema(File, Bindings0) ->
    Vars = [{'SchemaNodes', mnesia:table_info(schema, disc_copies)}],
    Bindings = 
	foldl(
	  fun({K, V}, Bs) ->
		  erl_eval:add_binding(K, V, Bs)
	  end, Bindings0, Vars),
    case file:script(File, Bindings) of
	{ok, Dictionary} ->
	    mnesia_schema:schema_transaction(
	      fun() ->
		      load_dictionary(Dictionary, [])
	      end);
	Error ->
	    Error
    end.

load_dictionary(Dict, Parent) when is_list(Dict) ->
    foreach(
      fun({table, Tab, Opts}) ->
	      case keysearch(rdbms, 1, Opts) of
		  false          -> do_create_table(Tab, Opts);
		  {value,{_,[]}} -> do_create_table(Tab, Opts);
		  {value, {_, [_|_] = ROpts}} ->
		      case keysearch(membership, 1, ROpts) of
			  false ->
			      do_create_table(Tab, Opts);
			  {value, {_, P}} when Parent =/= P, Parent =/= [] ->
			      abort({conflicting_membership, Tab});
			  {value, _} ->
			      Opts1 = lists:keyreplace(
					rdbms, 1, Opts,
					lists:keydelete(membership, 1, ROpts)),
			      do_create_table(Tab, Opts1)
		      end
	      end;
	 ({group, Group, Members}) ->
	      load_dictionary(Members, Group),
	      MemberNames = lists:map(
			      fun({table, T, _}) -> {table, T};
				 ({group, G, _}) -> {group, G}
			      end, Members),
	      rdbms_group:do_add_group(Group, MemberNames)
      end, Dict).



%% mnesia callbacks =====================================
%%   for each callback, an internal function is implemented.
%%   the internal functions are not exported, since they are only
%%   meant to be used for operations generated by the dictionary.
%%   No integrity checks are performed on the internal functions.


lock(ActivityId, Opaque, LockItem, LockKind) ->
    Module = case LockItem of
		 {record, T, _} -> get_module(T);
		 {table, T} -> get_module(T);
		 _ -> mnesia
	     end,
    Module:lock(ActivityId, Opaque, LockItem, LockKind).


write(Tid, Ts, Tab, Rec, LockKind) ->
    ?dbg("verify_write(~p, ~p)~n", [Tab, Rec]),
    VMod = ?vmod,
    validate_rec(Tab, Rec, VMod),
    do_write(Tid, Ts, Tab, Rec, LockKind, VMod),
    check_references(Tab, Rec, write, VMod).



do_write(ActivityId, Opaque, WTab, WRec, LockKind, VMod) ->
    AMod = access_module(WTab, VMod),
    AMod:write(ActivityId, Opaque, WTab, WRec, LockKind),
    rdbms_index:update_index(
      ActivityId, Opaque, WTab, write, WRec, LockKind, VMod).
    

%% non-exported function -- used by rdbms internally, no verification
%%
write(Tab, Rec, VMod) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    do_write(ActivityId, Opaque, Tab, Rec, write, VMod).

delete(Tid, Ts, Tab, Key, LockKind) ->
    delete(Tid, Ts, Tab, Key, LockKind, ?vmod).

delete(Tid, Ts, Tab, Key, LockKind, VMod) ->
    Objs = read(Tid, Ts, Tab, Key, LockKind, VMod),
    AMod = access_module(Tab, VMod),
    foreach(fun(Obj) ->
		    verify_delete_object(Tab, Obj, VMod),
		    AMod:delete_object(Tid, Ts, Tab, Obj, LockKind),
		    rdbms_index:update_index(
		      Tid, Ts, Tab, delete_object, Obj, LockKind, VMod)
	    end, Objs).


delete_object(Tab, Obj, VMod) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    delete_object(ActivityId, Opaque, Tab, Obj, write, VMod).


delete_object(ActivityId, Opaque, Tab, Obj, LockKind) ->
    VMod = ?vmod,
    delete_object(ActivityId, Opaque, Tab, Obj, LockKind, VMod).

delete_object(ActivityId, Opaque, Tab, Obj, LockKind, VMod) ->
    verify_delete_object(Tab, Obj, VMod),
    do_delete_object(ActivityId, Opaque, Tab, Obj, LockKind, VMod).


do_delete_object(Tid, Ts, Tab, Obj, LockKind, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:delete_object(Tid, Ts, Tab, Obj, LockKind),
    rdbms_index:update_index(Tid, Ts, Tab, delete_object, Obj, LockKind, VMod).


read(Tid, Ts, Tab, Key, LockKind) ->
    read(Tid, Ts, Tab, Key, LockKind, ?vmod).

read(Tid, Ts, Tab, Key, LockKind, VMod) ->
    AMod = access_module(Tab, VMod),
    on_read(Tab, AMod:read(Tid, Ts, Tab, Key, LockKind), VMod).

read(Tab, Key, VMod) ->
    {Tid, Ts} = get_mnesia_transaction_data(),
    read(Tid, Ts, Tab, Key, read, VMod).


match_object(ActivityId, Opaque, Tab, Pattern, _LockKind) ->
    VMod = ?vmod,
    AMod = access_module(Tab, VMod),
    AMod:match_object(ActivityId, Opaque, Tab, Pattern, read).

match_object(Tab, Pattern, VMod) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    VMod = ?vmod,
    AMod = access_module(Tab, VMod),
    AMod:match_object(ActivityId, Opaque, Tab, Pattern, read).

all_keys(ActivityId, Opaque, Tab, LockKind) ->
    VMod = ?vmod,
    AMod = access_module(Tab, VMod),
    AMod:all_keys(ActivityId, Opaque, Tab, LockKind).

index_match_object(ActivityId, Opaque, Tab, Pattern, Attr, LockKind) ->
    VMod = ?vmod,
    AMod = access_module(Tab, VMod),
    AMod:index_match_object(ActivityId, Opaque, Tab, 
			    Pattern, Attr, LockKind).

index_read(Tid, Ts, Tab, SecondaryKey, Attr, LockKind) ->
    index_read(Tid, Ts, Tab, SecondaryKey, Attr, LockKind, ?vmod).

index_read(Tid, Ts, Tab, SecondaryKey, Attr, LockKind, VMod) ->
    rdbms_index:read(Tid, Ts, Tab, SecondaryKey, Attr, LockKind, VMod).

index_read(Tab, SecondaryKey, Attr, VMod) ->
    {Tid, Ts} = get_mnesia_transaction_data(),
    index_read(Tid, Ts, Tab, SecondaryKey, Attr, VMod).


table_info(_ActivityId, _Opaque, Tab, InfoItem) ->
    VMod = ?vmod,
    table_info(Tab, InfoItem, VMod).


first(Tid, Ts, Tab) ->
    first(Tid, Ts, Tab, ?vmod).

first(Tid, Ts, Tab, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:first(Tid, Ts, Tab).

last(Tid, Ts, Tab) ->
    last(Tid, Ts, Tab, ?vmod).

last(Tid, Ts, Tab, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:last(Tid, Ts, Tab).

next(Tid, Ts, Tab, Key) ->
    next(Tid, Ts, Tab, Key, ?vmod).
    
next(Tid, Ts, Tab, Key, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:next(Tid, Ts, Tab, Key).

prev(Tid, Ts, Tab, Key) ->
    prev(Tid, Ts, Tab, Key, ?vmod).
    
prev(Tid, Ts, Tab, Key, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:prev(Tid, Ts, Tab, Key).

foldl(Tid, Ts, Fun, Acc, Tab, LockKind) ->
    foldl(Tid, Ts, Fun, Acc, Tab, LockKind, ?vmod).

foldl(Tid, Ts, Fun, Acc, Tab, LockKind, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:foldl(Tid, Ts, Fun, Acc, Tab, LockKind).

foldr(Tid, Ts, Fun, Acc, Tab, LockKind) ->
    foldr(Tid, Ts, Fun, Acc, Tab, LockKind, ?vmod).

foldr(Tid, Ts, Fun, Acc, Tab, LockKind, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:foldr(Tid, Ts, Fun, Acc, Tab, LockKind).



select(Tid, Ts, Tab, Pat, LockKind) ->
    do_select(Tid, Ts, Tab, Pat, LockKind, ?vmod).

do_select(Tid, Ts, Tab, Pat, LockKind, VMod) ->
    AMod = access_module(Tab, VMod),
    AMod:select(Tid, Ts, Tab, Pat, LockKind).

select(Tid, Ts, Tab, Pat, NObjects, LockKind) ->
    do_select(Tid, Ts, Tab, Pat, NObjects, LockKind, ?vmod).

do_select(Tid, Ts, Tab, Pat, NObjects, LockKind, VMod) ->
    AMod = access_module(Tab, VMod),
    case AMod:select(Tid, Ts, Tab, Pat, NObjects, LockKind) of
	'$end_of_table' = Result ->
	    Result;
	{Objs, Cont} ->
	    {Objs, {AMod, fun(Tid1, Ts1) ->
				  AMod:select_cont(Tid1, Ts1, Cont)
			  end}}
    end.


%%% Cont is required to be a fun/2 as returned from rdbms:do_select/7 above.
%%%
select_cont(Tid, Ts, {AMod, Cont}) when is_function(Cont, 2) ->
    case Cont(Tid, Ts) of
	{Objs, Cont1} ->
	    {Objs, {AMod, fun(Tid1, Ts1) ->
				  AMod:select_cont(Tid1, Ts1, Cont1)
			  end}};
	'$end_of_table' ->
	    '$end_of_table'
    end.


onload_fun(Table, LoadReason) ->
    rdbms_index:index_init_fun(Table, LoadReason).

%% end mnesia callbacks =====================================


%%% fetch_objects(Tab, Attr, Keys) ->
%%%     fetch_objects(Tab, Attr, Keys, ?vmod).

fetch_objects(Tab, Attr, Keys, VMod) ->
    case key_type(Tab, Attr, VMod) of
	primary ->
	    lists:flatmap(
	      fun(K) ->
		      read(Tab, K, VMod)
	      end, Keys);
	secondary ->
	    lists:flatmap(
	      fun(K) ->
		      opt_index_read(Tab, K, Attr, VMod)
	      end, Keys);
	attribute ->
	    Wild = table_info(Tab, wild_pattern, VMod),
	    Apos = attribute_position(Tab, Attr, VMod),
	    Pat = [{setelement(Apos, Wild, Key), [], ['$_']} ||
		      Key <- Keys],
	    select(Tab, Pat, VMod);
%%%	    Found = match_object(Tab, setelement(Apos, Wild, Key), VMod);
	{compound, Attrs} ->
	    Wild = table_info(Tab, wild_pattern, VMod),
	    Pat =
		[{build_compound_pattern(Attrs,Key,Wild,Tab,VMod),[],['$_']} ||
		    Key <- Keys],
	    select(Tab, Pat, VMod)
%%% 	    match_object(
%%% 	      Tab, build_compound_pattern(
%%% 		     Attrs, Key, Wild, Tab, VMod), VMod)
    end.

select(Tab, Pattern, VMod) ->
    {Tid, Ts} = get_mnesia_transaction_data(),
    VMod = ?vmod,
    AMod = access_module(Tab, VMod),
    AMod:select(Tid, Ts, Tab, Pattern, write).



opt_index_read(Tab, Key, Attr, VMod) ->
    index_read(Tab, Key, Attr, VMod).


%% build_compound_pattern([AttrName], Key, WildPattern, TableName)
%%
%% This is used when fetching objects based on a compound attribute
%% Example: Given a record #person{surname, lastname, ...}, and if
%% Example: 'name' is defined as a compound attribute: [surname, lastname],
%% we can specify 'name' := ["ulf", "wiger"], and this function will translate
%% it to #person{surname = "ulf", lastname = "wiger"}.
%%
build_compound_pattern(Attrs, Key, Wild, Tab, VMod) when tuple(Key) ->
    build_compound_pattern1(Attrs, tuple_to_list(Key), Wild, Tab, VMod);
build_compound_pattern(Attrs, Key, Wild, Tab, VMod) ->
    build_compound_pattern1(Attrs, Key, Wild, Tab, VMod).

build_compound_pattern1([Attr|Attrs], [Value|Vals], Pat, Tab, VMod) ->
    Pos = attribute_position(Tab, Attr, VMod),
    build_compound_pattern1(
      Attrs, Vals, setelement(Pos, Pat, Value), Tab, VMod);
build_compound_pattern1([], [], Pat, _, _) ->
    Pat.
    

%% null_value()
%% 
%% Erlang doesn't have a real null value that can be used.
%% I think this is a pity, but not enough other people agreed...
%%
%% The null value is not configureable, but I don't consider this a big 
%% problem. Having a dynamic null value could cause severe problems, and would
%% also incur extra overhead.
%%
null_value() -> ?NULL.


mk_oid(_Tab, _Attr) ->
    {node(), erlang:now()}.

%% if functions are called during a transaction which have side-effects,
%% these functions may be used to "commit" or "undo" the effect.
%% multiple calls can be made during one transaction; the functions will
%% be called LIFO *after* the transaction is aborted or commited.
%%
%% TODO - changed call order to FIFO
register_rollback_action(Fun) ->
    register_action(rollback, Fun).

register_commit_action(Fun) ->
    register_action(commit, Fun).


%%%----------------------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%%----------------------------------------------------------------------



%%%----------------------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% #3.3.1   Code for Additional Actions (post-transaction triggers).
%%%----------------------------------------------------------------------

register_action(Type, Fun) when Type==rollback; Type==commit ->
    case get(mnesia_activity_state) of
	{_M, _Tid, Ts} ->
	    #rdbms_activity{funs = Funs} = A =
		get_activity_state(),
	    Funs1 = [{Fun, Ts#tidstore.level, Type}|Funs],
	    put_activity_state(A#rdbms_activity{funs = Funs1});
	undefined ->
	    exit(no_transaction)
    end.

get_activity_state() ->
    get(rdbms_activity_state).

put_activity_state(#rdbms_activity{} = State) ->
    put(rdbms_activity_state, State);
put_activity_state(Other) ->
    exit({bad_activity_state, Other}).

erase_activity_state() ->
    erase(rdbms_activity_state).


catch_run(F, Type) ->
    case catch F() of
	{'EXIT', Reason} ->
	    error_logger:error_report([{?MODULE, caught_exception},
				       {additional_action, Type},
				       {'EXIT', Reason}]),
	    ok;
	_ ->
	    ok
    end.


%%%----------------------------------------------------------------------
%%% #3.3.1   Code for RDBMS verification functions.
%%%----------------------------------------------------------------------


%%% verify_write(Tab, Rec) ->
%%%     validate_rec(Tab, Rec, ?vmod).

validate_rec(Tab, Rec, ?JIT_MODULE) ->
    try ?JIT_MODULE:validate_rec(Tab, Rec)
    catch
	error:Reason ->
	    erlang:error({Reason, erlang:get_stacktrace()})
    end;
validate_rec(Tab, Rec, {rdbms_verify, VRec}) ->
    rdbms_verify:validate_rec(Tab, Rec, VRec).

access_module(Tab, ?JIT_MODULE) ->
    ?JIT_MODULE:module(Tab);
access_module(Tab, {rdbms_verify, VRec}) ->
    rdbms_verify:module(Tab, VRec).

check_access(Tab, Obj, Mode, VMode) ->
    rdbms_verify:check_access(acl(Tab, VMode), Mode, Obj, Tab).

on_read(Tab, Objs, ?JIT_MODULE) ->
    ?JIT_MODULE:on_read(Tab, Objs);
on_read(Tab, Objs, {rdbms_verify,VRec}) ->
    rdbms_verify:on_read(Objs, Tab, VRec).

acl(Tab, ?JIT_MODULE) ->
    ?JIT_MODULE:acl(Tab);
acl(Tab, {rdbms_verify, VRec}) ->
    rdbms_verify:acl(Tab, VRec).

references(Tab, ?JIT_MODULE) ->
    ?JIT_MODULE:references(Tab);
references(Tab, {rdbms_verify, VRec}) ->
    rdbms_verify:references(Tab, VRec).

%%% rec_type(Tab, ?JIT_MODULE) ->
%%%     ?JIT_MODULE:rec_type(Tab);
%%% rec_type(Tab, {rdbms_verify, VRec}) ->
%%%     rdbms_verify:rec_type(Tab, VRec).

%%% acl(Tab, ?JIT_MODULE) ->
%%%     ?JIT_MODULE:acl(Tab);
%%% acl(Tab, {rdbms_verify, VRec}) ->
%%%     rdbms_verify:acl(Tab, VRec).

%%% global_types(?JIT_MODULE) ->
%%%     ?JIT_MODULE:global_types();
%%% global_types({rdbms_verify, #verify{is_schema_trans = IsSchemaTrans}}) ->
%%%     if IsSchemaTrans ->
%%% 	    rdbms_props:schema_global_types();
%%%        true ->
%%% 	    rdbms_props:global_types()
%%%     end.



attr_property(Tab, Attr, Prop, VMod) ->
    attr_property(Tab, Attr, Prop, VMod, undefined).

attr_property(Tab, Attr, Prop, ?JIT_MODULE, Default) ->
    ?JIT_MODULE:attr_property(Tab, Attr, Prop, Default);
attr_property(Tab, Attr, Prop, {_, #verify{attr_property = AP}}, Default) ->
    AP(Tab, Attr, Prop, Default).


table_info(Tab, InfoItem, ?JIT_MODULE) ->
    ?JIT_MODULE:table_info(Tab, InfoItem);
table_info(Tab, InfoItem, {_, #verify{table_info = TI}}) ->
    TI(Tab, InfoItem).


fetch_verification_module() ->
    case get_activity_state() of
	undefined ->
	    {rdbms_verify, vrec(false)};
	#rdbms_activity{verification_module = VMod} ->
	    VMod
    end.

default_verification_module() ->
    {rdbms_verify, vrec(false)}.

verification_module() ->
    IsSchemaTrans = is_schema_transaction(),
    Mod = 
	case IsSchemaTrans of
	    true ->
		{rdbms_verify, vrec(true)};
	    false ->
		Jit = ?JIT_MODULE,
		case erlang:module_loaded(Jit) of
		    true ->
			Jit;
		    false ->
			{rdbms_verify, vrec(false)}
		end
	end,
    {IsSchemaTrans, Mod}.

is_schema_transaction() ->
    case process_info(self(), initial_call) of
	{_, {mnesia_schema, schema_coordinator, _}} ->
	    true;
	_ ->
	    false
    end.

%%% IDEA: Two "verification modules": ?JIT_MODULE, and rdbms_verify.
%%% rdbms_verify takes a verification record containing funs to access
%%% metadata. These funs are different depending on whether the transaction
%%% is a schema transaction or a normal transaction. 

%%%

vrec(_IsSchemaTrans = false) ->
    {Tid,Ts} = get_tid_ts(),
    #verify{is_schema_trans = false,
	    tab_property = fun(Tab, P, Default) ->
				   rdbms_props:table_property(Tab, P, Default)
			   end,
	    attr_property = fun(Tab, A, P, Def) ->
				    rdbms_props:attr_property(Tab, A, P, Def)
			    end,
	    global_property = fun(P, Def) ->
				      rdbms_props:global_property(P, Def)
			      end,
	    table_info = fun(Tab, I) ->
				 mnesia:table_info(Tid, Ts, Tab, I)
			 end};
vrec(_IsSchemaTrans = true) ->
    #verify{is_schema_trans = true,
	    tab_property = fun(Tab, P, Def) ->
				   rdbms_props:schema_table_property(
				     Tab, P, Def)
			   end,
	    attr_property = fun(Tab, Attr, P, Def) ->
				    rdbms_props:schema_attr_property(
				      Tab, Attr, P, Def)
			    end,
	    global_property = fun(P, Def) ->
				      rdbms_props:schema_global_property(
					P, Def)
			      end,
	    table_info = fun(Tab, I) ->
				 rdbms_props:schema_table_info(Tab, I)
			 end}.

get_tid_ts() ->
    case get(mnesia_activity_state) of
	undefined ->
	    {{async,self()}, non_transaction};
	{_, Tid, Ts} ->
	    {Tid, Ts}
    end.

get_module(Tab) ->
    case mnesia:table_info(Tab, frag_properties) of
	[] ->
	    mnesia;
	[_|_] ->
	    mnesia_frag
    end.


%% verify_delete(Tab, Key, DelF)
%% This is called in order to verify a mnesia:delete({Tab, Key})
%% It triggers verification of referential integrity.
%%
%%% verify_delete(Tab, Key) ->
%%%     verify_delete(Tab, Key, ?vmod).

%%% verify_delete(Tab, Key, VMod) ->
%%%     Objs = read(Tab, Key, VMod),
%%%     foreach(fun(Obj) -> verify_delete_object(Tab, Obj, VMod) end, Objs).



%% verify_delete_object(Tab, Obj)
%% Similar to verify_delete(), but for delete_object/1
%%
%%% verify_delete_object(Tab, Obj) when record(Obj, rdbms_obj) ->
%%%     verify_delete_object(Tab, rdbms_obj_to_record(Obj));
%%% verify_delete_object(Tab, Obj) ->
%%%     verify_delete_object(Tab, Obj, ?vmod).


verify_delete_object(Tab, Obj, VMod) ->
    check_access(Tab, Obj, delete, VMod),
    check_references(Tab, Obj, delete, VMod).


%% violation(Which, Data)
%% called if an integrity check fails (not consistently used yet)
%% Which : which type of check was violated
%% Data  : helpful information
%%
violation(Which, Data) ->
    ?dbg("~p violation (~p)~n", [Which, Data]),
    exit({violation, {Which, Data}}).



%% 
%% Functions for accessing metadata.
%% -- metadata values are stored in the table 'sysMnesiaDict'
%%    which has the structure -record(sysMnesiaDict, {key, value}).
%%    where key ::= {Obj_type, Obj, Type}, e.g. {attr, {Table, Attr}, Type},
%%    {attr, Attr, Type}, {table, Table, Type}, or {record, Rec, Type}.
%%    


%%
%% check_references(Tab, Attribute, Value, Context) -> [RelatedObject]
%%
%% This function checks referential integrity.
%% Referential integrity rules are stored as metadata in the following way:
%% {{attr, {Tab, Attr}, references}, [{Tab2, Attr2, RefActions}]}, where
%% Tab    : the referencing table
%% Attr   : the referencing attribute
%% Tab2   : the referenced table
%% Attr2  : the referenced attribute(s) - 
%%          atom() | {atom()} | function(Object, Value)
%% RefActions : {Match, DeleteAction : Action, UpdateAction : Action}
%% Match  : handling of null values - partial | full
%% Action : referential action - 
%%		no_action | cascade | set_default | set_null
%%

check_references(Tab, Rec, Context, VMod) ->
    case references(Tab, VMod) of
	[] ->
	    true;
	[_|_] = Refs ->
	    Log = get_refs_log(),
	    foreach(
	      fun({How, What, Refs1}) ->
		      Values =
			  case How of
			      attr ->
				  [attribute_value(Tab, What, Rec, VMod)];
			      index ->
				  rdbms_index:index_values(
				    Tab,What,Rec,VMod);
			      eval ->
				  {Mod, Fun, Arg} = What,
				  Mod:Fun(Rec, Arg)
			  end,
		      foreach(
			fun({_Tab2, _Attr2, {_,_,ignore}})
			   when Context==write ->
				true;
			   ({_Tab2, _Attr2, {_,ignore,_}})
			   when Context==delete ->
				true;
			   ({Tab2, Attr2, Actions}) ->
				do_follow_refs(
				  Tab2,Attr2,Actions,
				  Tab, How, What, Values, Context,
				  Log, VMod)
			end, Refs1)
	      end, Refs),
	    true
    end.

do_follow_refs(Tab2, {via_index, Ix}=Via, Actions,
	       _Tab, _How, _What, Values, Context, Log, VMod) ->
    {Tid, Ts} = get_mnesia_transaction_data(),
    Objs = lists:flatmap(
	    fun(Value) ->
		    index_read(Tid, Ts, Tab2, Value, Ix, write, VMod)
	    end, Values),
    perform_ref_actions(
      Actions, Objs, Context, Tab2, Via, Values, Log, VMod);
do_follow_refs(Tab2, Attr2, Actions,
	       Tab, How, What, Values, Context, Log, VMod) ->
    {Match, _, _} = Actions,
    Objs = ref_match(Tab2, Attr2, Match, Values, Tab, How, What, VMod),
    perform_ref_actions(
      Actions, Objs, Context, Tab2, Attr2, Values, Log, VMod).


%%% inspect_references(Refs, Tab, Obj, delete, VMod) ->
%%%     %% to avoid a possible endless loop, delete Obj first
%%%     do_delete_object(Tab, Obj, VMod),
%%%     foreach(
%%%       fun({Attr, Refs1}) ->
%%% 	      foreach(
%%% 		fun({_Tab2, _Attr2, {_
%%% 		fun({Attr,Val, Refs1}) ->
%%% 			do_follow_refs(
%%% 			follow_refs(Refs1, Val, delete, Tab, Attr) ++ Acc
%%% 		end, [], Refs1)
%%%       end, Refs).


%%% perform_ref_actions(_, Objs, read, _, _, _, Acc, _) ->
%%%     lists:foldl(fun(O, A) -> [O|A] end, Acc, Objs);
perform_ref_actions(Actions, Objs, delete, Tab2, Attr2, Val, Log, VMod) ->
    {_Match, OnDelete, _OnUpdate} = Actions,
    case OnDelete of
	cascade ->
	    foreach(
	      fun(Obj) ->
		      log_ref(Tab2, Obj, delete, Log),
		      delete_object(Tab2, Obj, VMod)
	      end, Objs),
	      true;
	set_null ->
	    cascade_update(Objs, Tab2, Attr2, ?NULL, Log, VMod);
	set_default ->
	    cascade_update(Objs, Tab2, Attr2,
			   default(Tab2, Attr2, VMod), Log, VMod);
	no_action ->
	    if Objs == [] ->
		    [];
	       true ->
		    violation(ref_integrity, {delete, [Tab2,Attr2,Val]})
	    end
    end;
perform_ref_actions(Actions, Objs, write, Tab2, Attr2, Val, Log, VMod) ->
    {_Match, _OnDelete, OnUpdate} = Actions,
    case OnUpdate of
	cascade ->
	    cascade_update(Objs, Tab2, Attr2, Val, Log, VMod);
	set_null ->
	    cascade_update(Objs, Tab2, Attr2, ?NULL, Log, VMod);
	set_default ->
	    cascade_update(Objs, Tab2, Attr2,
			   default(Tab2, Attr2, VMod), Log, VMod);
	no_action ->
	    %% here we should probably also use the MATCH condition...
	    %% but since MATCH only matters for composite values, we skip it
	    %% for now.
	    if Objs == [] ->
		    violation(ref_integrity, {write, [Tab2, Attr2, Val]});
	       true ->
		    true
	    end
    end.


%% ref_match(...)
%%
%% This has to do with the handling of null values.
%% If the attribute used in the referencing action is a compound attribute,
%% we must deal with the issue of partial matches 
%% - thus the (Match : full | partial) stuff
%% For a more exhaustive explanation, see the SQL Standard
%%
ref_match(Tab2, Attr2, _Match, Val, _Tab1, _H, _W, VMod) when atom(Attr2) ->
    fetch_objects(Tab2, Attr2, Val, VMod);
ref_match(Tab2, Attr2, Match, Vals,
	  _Tab1, _H, _W, VMod) when is_list(Attr2) ->
    Wild = table_info(Tab2, wild_pattern, VMod),
    Objs = match_object(Tab2, Wild, VMod),
    AttrL = [attribute_position(Tab2, A, VMod) || A <- Attr2],
    lists:flatmap(
      fun(V) ->
	      ValL = tuple_to_list(V),
	      case {has_nulls(ValL), Match} of
		  {all, partial} -> Objs;
		  {some, full} ->   Objs;
		  _ ->       ref_compound_match(Objs, AttrL, ValL)
	      end
      end, Vals);
ref_match(Tab2, AttrF, _, Vals, _, _, _, VMod) when function(AttrF) ->
    %% TODO optimize. Check whether something more efficient than full
    %% linear search is possible (e.g. if target is a bag table.)
    Wild = table_info(Tab2, wild_pattern, VMod),
    Objs = match_object(Tab2, Wild, VMod),
    lists:foldl(
      fun(Obj, Acc) ->
	      lists:foldl(
		fun(Val, Acc1) ->
			case AttrF(Obj, Val) of
			    true ->
				[Obj|Acc1];
			    false ->
				Acc1
			end
		end, Acc, Vals)
      end, [], Objs).



ref_compound_match([O|Objs], Attrs, Vals) ->
    case match_compound_obj(O, Attrs, Vals) of
	true -> [O|ref_compound_match(Objs, Attrs, Vals)];
	false -> ref_compound_match(Objs, Attrs, Vals)
    end;
ref_compound_match([], _, _) -> [].

match_compound_obj(Obj, [_|Attrs], [?NULL|Vals]) ->
    match_compound_obj(Obj, Attrs, Vals);
match_compound_obj(Obj, [A|Attrs], [V|Vals]) ->
    if element(A, Obj) == V ->
	    match_compound_obj(Obj, Attrs, Vals);
       true ->
	    false
    end;
match_compound_obj(_, _, _) -> true.
	    


%% We allow for a 'fun' version of match
%% F(Object) should return true or false.
%%
%%% ref_fun_match([Obj|Objs], F, Value) ->
%%%     case F(Obj, Value) of
%%% 	true -> [Obj|ref_fun_match(Objs, F, Value)];	    
%%% 	false -> ref_fun_match(Objs, F, Value)
%%%     end;
%%% ref_fun_match([], _, _) -> [].



%% has_nulls([Value]) -> none | all | some
%% This function tells whether a list of values contains nulls.
%%
has_nulls([_|_] = Vals) ->
    Res = lists:foldl(fun(X, {_,Miss}) when X==?NULL  -> {true,Miss};
			 (X, {Hit, _}) when X=/=?NULL -> {Hit,true}
		      end, {false, false}, Vals),
    case Res of
	{true, true} -> some;
	{true, false} -> all;
	{false,true} -> none
    end.
%%% has_nulls([?NULL|Vals]) -> has_nulls(Vals, all);
%%% has_nulls([_|Vals]) -> has_nulls(Vals, none);
%%% has_nulls([]) -> all.

%%% has_nulls([?NULL|T], all) -> has_nulls(T, all);
%%% has_nulls([?NULL|T], none) -> has_nulls(T, some);
%%% has_nulls([_|T], all) -> has_nulls(T, some);
%%% has_nulls([_|T], Acc) -> has_nulls(T, Acc);
%%% has_nulls([], Acc) -> Acc.


%% cascade_update(...)
%%
%% Used in connection with referential integrity checks
%% set_default | set_null | (cascading update)
cascade_update(Objs, Tab, Attr, Value, Log, VMod) ->
    Attrs = table_info(Tab, attributes, VMod),
    Pos = pos(Attrs, Attr, 2, Tab),
    foreach(
      fun(Obj) ->
	      log_ref(Tab, Obj, write, Log),
	      write(Tab, setelement(Pos, Obj, Value), VMod)
      end, Objs).


new_refs_log() ->
    ets:new(refs_log, [set]).


get_refs_log() ->
    case get_activity_state() of
	#rdbms_activity{refs_logs = [Log|_]} ->
	    Log;
	#rdbms_activity{refs_logs = []} = A ->
	    Log = new_refs_log(),
	    put_activity_state(A#rdbms_activity{refs_logs = [Log]}),
	    Log
    end.

log_ref(Tab, Obj, Op, Log) when Op==write; Op==delete ->
    Key = element(2, Obj),
    case ets:insert_new(Log, {{Tab, Key, Op}}) of
	false ->
	    mnesia:abort({cyclical_reference, {Op, [Tab, Key]}});
	true ->
	    true
    end.

    

%%% ==========================================================
%%% Data Representation
%%%
%%% We define a data representation called rdbms_obj()
%%% (defined in rdbms.hrl)
%%%
%%% make_object(atom() | record() | rdbms_obj()) -> rdbms_obj()
%%% For all attributes where a value is not provided, specified defaults
%%% will be inserted.
%%% ==========================================================
%%% make_object(Tab) ->
%%%     make_object(Tab, []).

%%% make_object(Tab, Values) when atom(Tab) ->
%%%     VMod = ?vmod,
%%%     AttrNames = table_info(Tab, attributes, VMod),
%%%     Attrs = [{N, table_info({Tab,N}, type, VMod),
%%% 	      table_info({Tab,N}, default, VMod)} || N <- AttrNames],
%%%     replace_values(Values, #rdbms_obj{name = Tab, attributes = Attrs});
%%% make_object(Data, Values) ->
%%%     case is_rdbms_obj(Data) of
%%% 	true -> replace_values(Values, Data);  % no need to convert
%%% 	false ->
%%% 	    replace_values(Values, record_to_rdbms_obj(Data))
%%%     end.

make_simple_form(Tab) ->
    VMod = ?vmod,
    case attributes(Tab, VMod) of
	undefined -> 
	    mnesia:abort({no_exists, Tab, simple_form});
	AttrNames ->
	    Props = table_info(Tab, user_properties, VMod),
	    Refs = proplists:get_value(references, Props, []),
	    Acl = proplists:get_value(acl, Props, []),
	    RecType = proplists:get_value(rec_type, Props, []),
	    TypeDefs = [{T,D} || {{typedef,T},D} <- Props],
		Content = [{attrs, [], 
			foldr(
			  fun(N, Acc) ->
				  [{N, describe_attribute(Tab, N, VMod) ++
				    [{references,
				      describe_references(N, Refs)}], []}|Acc]
			  end, [], AttrNames)},
			   {acl, [], Acl},
			   {rec_type, [], RecType},
			   {typedefs, [], TypeDefs},
			   {external_copies, [],
			    table_info(Tab, external_copies, VMod)}],
	    {Tab, describe_generic_attributes(Tab, VMod), Content}
    end.

describe_attribute(Tab, Attr, VMod) ->
    [
     {type, attr_property(Tab, Attr, type, VMod)},
     {key_type, key_type(Tab, Attr, VMod)},
     {default, attr_property(Tab, Attr, default, VMod)}].

%%% describe_global_type(Attr, VMod) ->
%%%     [{What, global_property({attr, Attr, What}, VMod)} ||
%%% 	What <- [type, key_type, required, bounds, default, references]].

describe_generic_attributes(Tab, VMod) ->
    [{A, table_info(Tab, A, VMod)} || A <- [record_name,
					    attributes,
					    ram_copies,
					    disc_copies,
					    disc_only_copies,
					    local_content,
					    index,
					    snmp]].
describe_references(Name, Refs) ->    
    case lists:keysearch(Name, 1, Refs) of
	{value, {_, R}} ->
	    describe_references(R);
	false ->
	    []
    end.

describe_references(Refs) ->
    lists:map(
      fun({Tab, Attr, Action}) ->
	      {Match, OnDelete, OnUpdate} = Action,
	      {Tab, Attr, [{match, Match},
			   {update, OnUpdate},
			   {delete, OnDelete}]}
	      end, Refs).
    
%% attribute_value(Tab, Attr, Object, VMod)
%%
%% This is used to figure out the value of an attribute.
%% Needed since the attribute could be compound.
attribute_value(Tab, SubAttrs, Object, VMod) when is_list(SubAttrs) ->
    [element(attribute_position(Tab, A, VMod), Object) ||
	A <- SubAttrs];
attribute_value(Tab, Attr, Object, VMod) when is_atom(Attr) ->
    element(attribute_position(Tab, Attr, VMod), Object).


%%
%% type ::= atom | list | tuple | string | number | integer | float |
%%          any | term | record
%%          
%%% type(Attr) ->
%%%     attr_property(Attr, type).


%%% access(_Type, _Attr) -> undefined.

%%
%% required ::= true | false
%% 
%%% required(Attr) ->
%%%     case type(Attr) of
%%% 	oid ->
%%% 	    %% We enforce required==true for attributes of type oid.
%%% 	    true;
%%% 	_ ->
%%% 	    case attr_property(Attr, required) of
%%% 		undefined ->
%%% 		    false;
%%% 		Other ->
%%% 		    Other
%%% 	    end
%%%     end.

%%
%% default ::= Value
%% 
%%% default(Tab, Attr) ->
%%%     default(Tab, Attr, ?vmod).

default(Tab, Attr, VMod) ->
    case attr_property(Tab, Attr, default, VMod) of
	undefined ->
	    case attr_property(Tab, Attr, type, VMod) of
		{record, Rec} ->
		    default_record(Rec, VMod);
		oid ->
		    {node(), erlang:now()};
		_ ->
		    ?NULL
	    end;
	Val -> Val
    end.

default_record(Tab, VMod) ->
    Defs = [default(Tab, Attr, VMod) || Attr <- attributes(Tab, VMod)],
    list_to_tuple([Tab|Defs]).

%% position(Attribute, Record) -> integer()
%% This calculates the element position of a specified attribute
%% Note that compound attributes have no position
%% position(CompoundAttr) will result in exit({invalid_attribute, ...})
%%
%%% position(Attr, Rec) ->
%%%     pos(attributes(Rec), Attr, 1, Rec).

pos([Attr|_], Attr, N, _Tab) ->
    N;
pos([_|T], Attr, N, Tab) ->
    pos(T, Attr, N+1, Tab);
pos([], Attr, _, Tab) ->
    violation(type, {invalid_attribute, {Tab, Attr}}).

%%
%% attrs ::= [attr]
%% -- list of attrs in correct order
%% 
%%% attributes(Tab) ->
%%%     attributes(Tab, ?vmod).

attributes(Tab, VMod) ->
    case catch table_info(Tab, attributes, VMod) of
	{'EXIT', _} ->
	    global_property({record, Tab, attributes}, VMod);
	Attrs when list(Attrs) ->
	    Attrs
    end.

%% all_attributes(Tab)
%%    {PhysicalAttributes, LogicalAttributes}
%% where
%%    LogicalAttributes ::= [{Attr, compound, [SubAttr]}]
%%
%%% all_attributes(Tab) ->
%%%     VMod = ?vmod,
%%%     PhysicalAttrs = attributes(Tab, VMod),
%%%     case catch table_info(Tab, user_properties, VMod) of
%%% 	{'EXIT', _} ->
%%% 	    {PhysicalAttrs, []};
%%% 	Props ->
%%% 	    CompoundAttrs = 
%%% 		[{A, {compound, Sub}} || 
%%% 		    {{attr,A,type},{compound,Sub}} <- Props],
%%% 	    {PhysicalAttrs, CompoundAttrs}
%%%     end.




attribute_position(Tab, Attr, VMod) ->
    pos(table_info(Tab, attributes, VMod), Attr, 2, Tab).

%% key_type(Tab, Attribute, VMod)
%% This checks whether Attribute of Tab is a key, and if so, which type of key
%% Possible return values are:
%% - primary    - the primary (unique) key
%% - secondary  - non-unique key (has an index attached to it)
%% - {compound, Sub} - compound attribute (may contain keys)
%% - attribute  - not a key
key_type(Tab, Attr, VMod) ->
    case attr_property(Tab, Attr, key_type, VMod) of
	undefined ->
	    case attr_property(Tab, Attr, type, VMod) of
		{compound, SubAttrs} ->
		    {compound, SubAttrs};
		_ ->
		    Apos = attribute_position(Tab, Attr, VMod),
		    case (Apos == ?KEYPOS) of
			true -> primary;
			false -> 
			    case table_info(Tab, index, VMod) of
				%% TODO - also check extended indexes
				[] -> attribute;
				Ix ->
				    case lists:member(Apos, Ix) of
					true -> secondary;
					false ->
					    attribute
				    end
			    end
		    end
	    end;
	Type ->
	    Type
    end.


%%% attr_property(Tab, Attr, Prop) ->
%%%     rdbms_props:attr_property(Tab, Attr, Prop).



global_property(Prop, ?JIT_MODULE) ->
    ?JIT_MODULE:global_property(Prop);
global_property(Prop, {_, #verify{global_property = GP}}) ->
    GP(Prop).


get_mnesia_transaction_data() ->
    case get(mnesia_activity_state) of
        {_, ActivityId, Opaque} ->
           {ActivityId, Opaque};
        _ -> 
	    abort(no_transaction)
    end.

%%%----------------------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%%----------------------------------------------------------------------


