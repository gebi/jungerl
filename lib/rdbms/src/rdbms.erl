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


-ifdef(debug).
-define(dbg(Fmt, Args), io:format("~p-~p: " ++ Fmt, [?MODULE,?LINE|Args])).
-else.
-define(dbg(Fmt,Args), no_debug).
-endif.

-define(NULL, '#.[].#').  % this is my definition of a null value.
-define(KEYPOS, 2).

%%%----------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%%----------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%%----------------------------------------------------------------------

%% Substitute for mnesia:transaction/1.
-export([activity/1]).

%% Initializes the dictionary.
-export([add_properties/1,	% ([Prop])
	 do_add_properties/1,
	 do_add_properties/2,	% ([Prop]) called within a schema transaction
	 drop_references/1,
	 do_drop_references/1]).

-export([create_table/2,
	 do_create_table/2,
	 delete_table/1,
	 do_delete_table/1]).

%% Attribute metadata
-export([bounds/1,	% (Attr)
	 default/1,	% (Attr)
	 references/1,	% (Attr)
	 required/1,	% (Attr)
	 key_type/1,	% ({Tab, Attr})
	 key_type/2,	% (Tab, Attr)
	 set_property/2,	% (PropKey, Value)
	 type/1]).	% (Attr)

-export([verify_attribute/2,	% (Value, Attribute)
	 verify_type/2,		% (Value, Attribute)
	 verify_bounds/2]).	% (Value, Attribute)

-export([access/2]).  % NYI

%% Representation
-export([make_object/1,		% make an rdbms object, new or from existing
	 make_object/2,		% make/update an RDBMS object
	 make_simple_form/1,	% make a simple xmerl form
	 make_simple_form/2,	% make/update a simple xmerl form
	 make_record/1]).	% make an Erlang record from an RDBMS obj


%% Table metadata
-export([attributes/1,			% (Table)
	 all_attributes/1,		% (Table)
	 attribute/2,			% (Position, Table)
	 attribute_value/3,		% (Table, AttrName, Object)
	 position/2,			% (Attribute, Table)
	 default_record/1,		% (Table)
	 verify_write/2,		% (Table, Object)
	 verify_delete/2,		% (Table, Key)
	 verify_delete_object/2,	% (Table, Object)
	 action_on_read/1,		% (Table)
	 action_on_write/1,		% (Table)
	 register_commit_action/1,	% (function/0)
	 register_rollback_action/1]).	% (function/0)

-export([null_value/0]).

%% Update
-export([lock/4,
	 write/5,
	 delete/5,
	 delete_object/5,
	 read/5,
	 match_object/5,
	 all_keys/4,
	 index_match_object/6,
	 index_read/6,
	 table_info/4]).

%% extra retrieval functions
-export([select/4,		% (Tab, SelectAttr, SelectKey, ProjectAttrs)
	 fetch_objects/3]).	% (Tab, SelectAttr, SelectKey)

%%%----------------------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%%----------------------------------------------------------------------

-include("rdbms.hrl").

%%%----------------------------------------------------------------------
%%% #3.    CODE
%%%----------------------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%%----------------------------------------------------------------------


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


activity(Fun) ->
    %% We maintain our own transaction Id (for the "additional actions")
    Id = erlang:now(),
    F = fun() ->
		setup_transaction(Id),
		Fun()
	end,
    %% We perform a catch on a wrapped call to activity/1. This may look
    %% funny, but we want to allow {'EXIT', Reason} as a return value from
    %% activity/1 (which could happen if the last expression of the fun
    %% is a catch expression. This doesn't mean that the transaction was
    %% aborted in mnesia's eyes.
    Result = (catch {ok, mnesia:activity(transaction, F, [], ?MODULE)}),
    handle_result(Result, get_transaction_levels()).


%% handle result from activity/1 (above)
%%
handle_result(Result, [_]) ->
    %% top level result
    case Result of
	{'EXIT', Reason} ->
	    additional_action(rollback),
	    cleanup_transaction(),
	    exit(Reason);
	{ok, ReturnValue} ->
	    additional_action(commit),
	    cleanup_transaction(),
	    ReturnValue
    end;
handle_result(Result, [Level|_]) ->
    %% inner transaction result
    case Result of
	{'EXIT', Reason} ->
	    additional_action(rollback, Level),
	    cleanup_transaction(Level),
	    exit(Reason);
	{ok, ReturnValue} ->
	    pop_transaction_level(),
	    ReturnValue
    end.



create_table(Name, Opts) ->
    mnesia_schema:schema_transaction(
      fun() ->
	      do_create_table(Name, Opts)
      end).

do_create_table(Name, Opts) ->
    case lists:keysearch(rdbms, 1, Opts) of
	{value, {_, Props}} ->
	    Options = lists:keydelete(rdbms,1,Opts),
	    Cs = mnesia_schema:list2cs([{name,Name}|Options]),
	    mnesia_schema:do_create_table(Cs),
	    rdbms:do_add_properties(Props);
	false ->
	    Options = lists:keydelete(rdbms,1,Opts),
	    Cs = mnesia_schema:list2cs([{name,Name}|Options]),
	    mnesia_schema:do_create_table(Cs)
    end.

delete_table(Name) ->      
    mnesia_schema:schema_transaction(
      fun() ->
	      do_delete_table(Name)
      end).

do_delete_table(Name) ->
    mnesia_schema:do_delete_table(Name),
    do_drop_references(Name).




table_properties(Props, Tab) ->
    verify_table_properties(Props, Tab).


add_properties(Props) ->
    F = fun() ->
		do_add_properties(Props)
	end,
    mnesia_schema:schema_transaction(F).


do_add_properties(Props, Tab) ->
    do_add_properties(table_properties(Props, Tab)).

do_add_properties([{Key, Val}|T]) ->
    do_set_property(Key, Val),
    do_add_properties(T);
do_add_properties([]) ->
    ok.

drop_references(ToTab) ->
    mnesia_schema:schema_transaction(
      fun() ->
	      do_drop_references(ToTab)
      end).

do_drop_references(ToTab) ->
    lists:foreach(
      fun(Tab) ->
	      Props = mnesia:table_info(Tab, user_properties),
	      search_props(Props, ToTab, Tab)
      end, mnesia:system_info(tables)).

search_props([{{attr,Attr,references},Refs}|Props], ToTab, Tab) ->
    case [{T,A,As} || {T,A,As} <- Refs,
		      T == ToTab] of
	[] ->
	    %% no reference to ToTab
	    search_props(Props, ToTab, Tab);
	Remove ->
	    do_set_property({attr,{Tab,Attr},references}, Refs -- Remove),
	    search_props(Props, ToTab, Tab)
    end;
search_props([P|Ps], ToTab, Tab) ->
    search_props(Ps, ToTab, Tab);
search_props([], _, _) ->
    ok.

%% mnesia callbacks =====================================
%%   for each callback, an internal function is implemented.
%%   the internal functions are not exported, since they are only
%%   meant to be used for operations generated by the dictionary.
%%   No integrity checks are performed on the internal functions.


lock(ActivityId, Opaque, LockItem, LockKind) -> 
    mnesia:lock(ActivityId, Opaque, LockItem, LockKind).

%lock(LockItem, LockKind) ->
%    {ActivityId, Opaque} = get_mnesia_transaction_data(),
%    mnesia:lock(ActivityId, Opaque, LockItem, LockKind).

write(ActivityId, Opaque, Tab, Rec, LockKind) ->
    ?dbg("verify_write(~p, ~p)~n", [Tab, Rec]),
    {WTab, WRec} = if record(Rec, rdbms_obj) ->
			   {Rec#rdbms_obj.name, rdbms_obj_to_record(Rec)};
		      true ->
			   {Tab, Rec}
		   end,
    verify_write(WTab, WRec),
    do_write(ActivityId, Opaque, WTab, WRec, LockKind).


% do_write(Tab, Rec) ->
%     {ActivityId, Opaque} = get_mnesia_transaction_data(),
%     do_write(ActivityId, Opaque, Tab, Rec, write).

do_write(ActivityId, Opaque, WTab, WRec, LockKind) ->
    case action_on_write(WTab) of
	default ->
	    mnesia:write(ActivityId, Opaque, WTab, WRec, LockKind);
	F ->
	    F(WRec)
    end.
    

%% non-exported function -- used by rdbms internally, no verification
%%
write(Tab, Rec) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    do_write(ActivityId, Opaque, Tab, Rec, write).

delete(ActivityId, Opaque, Tab, Key, LockKind) ->
    verify_delete(Tab, Key).


% do_delete(Tab, Rec) ->
%     {ActivityId, Opaque} = get_mnesia_transaction_data(),
%     do_delete(ActivityId, Opaque, Tab, Rec, write).

% do_delete(ActivityId, Opaque, Tab, Key, LockKind) ->
%     case action_on_delete(Tab) of
% 	default ->
% 	    mnesia:delete(ActivityId, Opaque, Tab, Key, LockKind);
% 	F ->
% 	    F(key, Key)
%     end.



%delete(Tab, Key) ->
%    {ActivityId, Opaque} = get_mnesia_transaction_data(),
%    case action_on_delete(Tab) of
%	default ->
%	    mnesia:delete(ActivityId, Opaque, Tab, Key, write);
%	F ->
%	    F(key, Key)
%    end.
delete_object(Tab, Obj) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    delete_object(ActivityId, Opaque, Tab, Obj, write).


delete_object(ActivityId, Opaque, Tab, Obj, LockKind) ->
    verify_delete_object(Tab, Obj).


do_delete_object(ActivityId, Opaque, Tab, Obj, LockKind) ->
    case action_on_delete(Tab) of
	default ->
	    mnesia:delete_object(
	      ActivityId, Opaque, Tab, Obj, LockKind);
	F ->
	    F(object, Obj)
    end.



%% I want cascading delete to work through multiple levels, but
%% activating the uncommented line below may cause an endless loop.
do_delete_object(Tab, Obj) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
%     delete_object(ActivityId, Opaque, Tab, Obj, write).
    do_delete_object(ActivityId, Opaque, Tab, Obj, write).


read(ActivityId, Opaque, Tab, Key, LockKind) ->
    case action_on_read(Tab) of
	default ->
	    mnesia:read(ActivityId, Opaque, Tab, Key, LockKind);
	F ->
	    F(Key)
    end.

read(Tab, Key) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    case action_on_read(Tab) of
	default ->
	    mnesia:read(ActivityId, Opaque, Tab, Key, read);
	F ->
	    F(Key)
    end.


match_object(ActivityId, Opaque, Tab, Pattern, LockKind) ->
    mnesia:match_object(ActivityId, Opaque, Tab, Pattern, read).

match_object(Tab, Pattern) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    mnesia:match_object(ActivityId, Opaque, Tab, Pattern, read).


all_keys(ActivityId, Opaque, Tab, LockKind) ->
    mnesia:all_keys(ActivityId, Opaque, Tab, LockKind).

%all_keys(Tab) ->
%    {ActivityId, Opaque} = get_mnesia_transaction_data(),
%    mnesia:all_keys(ActivityId, Opaque, Tab, read).

index_match_object(ActivityId, Opaque, Tab, Pattern, Attr, LockKind) ->
    mnesia:index_match_object(ActivityId, Opaque, Tab, 
			      Pattern, Attr, LockKind).

%index_match_object(Tab, Pattern, Attr) ->
%    {ActivityId, Opaque} = get_mnesia_transaction_data(),
%    mnesia:index_match_object(ActivityId, Opaque, Tab, 
%			      Pattern, Attr, read).

index_read(ActivityId, Opaque, Tab, SecondaryKey, Attr, LockKind) ->
    mnesia:index_read(ActivityId, Opaque, Tab, SecondaryKey, Attr, LockKind).

index_read(Tab, SecondaryKey, Attr) ->
    {ActivityId, Opaque} = get_mnesia_transaction_data(),
    mnesia:index_read(ActivityId, Opaque, Tab, SecondaryKey, Attr, read).


table_info(ActivityId, Opaque, Tab, InfoItem) ->
    mnesia:table_info(ActivityId, Opaque, Tab, InfoItem).

table_info(Tab, InfoItem) ->
    mnesia:table_info(Tab, InfoItem).

%% end mnesia callbacks =====================================


select(Tab, Attr, Key, Attrs) when list(Attrs) ->
    select(fetch_objects(Tab, Attr, Key), Tab, Key, Attrs, list);
select(Tab, Attr, Key, Attrs) when tuple(Attrs) ->
    select(fetch_objects(Tab, Attr, Key), Tab, Key, 
	   tuple_to_list(Attrs), tuple).

select([Obj|Objs], Tab, Key, Attrs, list) ->
    Vals = [attribute_value(Tab, A, Obj) || A <- Attrs],
    [Vals|select(Objs, Tab, Key, Attrs, list)];
select([Obj|Objs], Tab, Key, Attrs, tuple) ->
    Vals = [attribute_value(Tab, A, Obj) || A <- Attrs],
    [list_to_tuple(Vals)|select(Objs, Tab, Key, Attrs, tuple)];
select([], _, _, _, _) ->
    [].


fetch_objects(Tab, Attr, Key) ->
    case key_type(Tab, Attr) of
	primary ->
	    read(Tab, Key);
	secondary ->
	    opt_index_read(Tab, Key, Attr);
	attribute ->
	    Wild = table_info(Tab, wild_pattern),
	    Apos = attribute_position(Tab, Attr),
	    match_object(Tab, setelement(Apos, Wild, Key));
	{compound, Attrs} ->
	    Wild = table_info(Tab, wild_pattern),
	    match_object(Tab, build_compound_pattern(Attrs, Key, Wild, Tab))
    end.

%% opt_index_read(Tab, Key, Attr)
%% This function should really allow for other kinds of index than the ones
%% made possible through mnesia (not implemented yet)
%%
opt_index_read(Tab, Key, Attr) ->
    index_read(Tab, Key, Attr).


%% build_compound_pattern([AttrName], Key, WildPattern, TableName)
%%
%% This is used when fetching objects based on a compound attribute
%% Example: Given a record #person{surname, lastname, ...}, and if
%% Example: 'name' is defined as a compound attribute: [surname, lastname],
%% we can specify 'name' := ["ulf", "wiger"], and this function will translate
%% it to #person{surname = "ulf", lastname = "wiger"}.
%%
build_compound_pattern(Attrs, Key, Wild, Tab) when tuple(Key) ->
    build_compound_pattern1(Attrs, tuple_to_list(Key), Wild, Tab);
build_compound_pattern(Attrs, Key, Wild, Tab) ->
    build_compound_pattern1(Attrs, Key, Wild, Tab).

build_compound_pattern1([Attr|Attrs], [Value|Vals], Pat, Tab) ->
    Pos = attribute_position(Tab, Attr),
    build_compound_pattern1(Attrs, Vals, setelement(Pos, Pat, Value), Tab);
build_compound_pattern1([], [], Pat, _) ->
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


%% if functions are called during a transaction which have side-effects,
%% these functions may be used to "commit" or "undo" the effect.
%% multiple calls can be made during one transaction; the functions will
%% be called LIFO *after* the transaction is aborted or commited.
%%
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

setup_transaction(Id) ->
    Key = trans_level_key(),
    case get(Key) of
	undefined ->
	    put(Key, [Id]);
	L = [Id|_] ->
	    L;	% this happens at transaction restart
	L ->
	    %% we're setting up an inner transaction -- push Id onto stack
	    put(Key, [Id|L])
    end,
    CommitKey = additional_action_key(commit, Id),
    RollbackKey = additional_action_key(rollback, Id),
    put(CommitKey, []),
    put(RollbackKey, []),
    ok.


register_action(Type, Fun) ->
    Key = additional_action_key(Type),
    case get(Key) of
	undefined ->
	    exit(no_transaction);
	[] ->
	    put(Key, [Fun]);
	Funs when list(Funs) ->
	    put(Key, [Fun|Funs])
    end.

additional_action_key(Type) ->
    additional_action_key(Type, transaction_level()).

additional_action_key(Type, Level) -> 
    {?MODULE, additional, Type, Level}.


transaction_level() ->
    [L|_] = get(trans_level_key()),
    L.

get_transaction_levels() ->
    case get(trans_level_key()) of
	undefined ->
	    exit(no_transaction);
	L ->
	    L
    end.

pop_transaction_level() ->
    Key = trans_level_key(),
    [H|T] = get(Key),
    put(Key, T).

trans_level_key() ->
    {?MODULE, trans_levels}.


additional_action(Type) ->
    Actions = get_actions(Type),
    do_run_actions(Actions, Type).

%% used to trigger registered commit/rollback actions
%%
additional_action(Type, Level) ->
    Key = additional_action_key(Type, Level),
    case get(Key) of
	[] -> ok;
	Funs when list(Funs) ->
	    erase(Key),
	    lists:foreach(fun(F) ->
				  catch_run(F, Type) 
			  end, lists:reverse(Funs))
    end.

do_run_actions([{Key, Funs}|T], Type) ->
    erase(Key),
    lists:foreach(fun(F) ->
			  catch_run(F, Type)
		  end, lists:reverse(Funs)),
    do_run_actions(T, Type);
do_run_actions([], Type) ->
    ok.

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


%% Clean up outer transaction -- and all its inner transactions.
cleanup_transaction() ->
    Actions = get_actions(),
    [erase(Key) || {Key, _} <- Actions],
    erase(trans_level_key()).

%% Clean up inner transaction
cleanup_transaction(Level) ->
    Actions = get_all_actions(Level),
    [erase(Key) || {Key, _} <- Actions],
    pop_transaction_level().


get_actions() ->
    [{K, V} || {K = {?MODULE,additional,_,_}, V} <- get()].

get_actions(Type) ->
    [{K, V} || {K = {?MODULE,additional,T,_}, V} <- get(),
               T == Type].

get_all_actions(Level) ->
    [{K, V} || {K = {?MODULE,additional,_,L}, V} <- get(),
               L == Level].


%%%----------------------------------------------------------------------
%%% #3.3.1   Code for RDBMS verification functions.
%%%----------------------------------------------------------------------


%% verify_write(Tab, Record)
%% This is called in order to verify a mnesia:write(Record)
%% It triggers verification of e.g. type, bounds, and referential integrity
%%
%% NOTE: mnesia:write/1 may actually be translated to a normal function call.
%% This is a dirty trick which should probably never be used.
%%
verify_write(Tab, Rec) when record(Rec, rdbms_obj) ->
    verify_record(Rec#rdbms_obj.name, rdbms_obj_to_record(Rec), write);
verify_write(Tab, Rec) ->
    verify_record(Tab, Rec, write).


%% verify_delete(Tab, Key, DelF)
%% This is called in order to verify a mnesia:delete({Tab, Key})
%% It triggers verification of referential integrity.
%%
verify_delete(Tab, Key) ->
    Objs = read(Tab, Key),
    verify_delete1(Objs, Tab).

verify_delete1([Obj|Objs], Tab) ->
    verify_delete_object(Tab, Obj),
    verify_delete1(Objs, Tab);
verify_delete1([], _) ->
    ok.


%% verify_delete_object(Tab, Obj)
%% Similar to verify_delete(), but for delete_object/1
%%
verify_delete_object(Tab, Obj) when record(Obj, rdbms_obj) ->
    verify_delete_object(Tab, rdbms_obj_to_record(Obj));
verify_delete_object(Tab, Obj) ->
    Attrs = attributes(Tab),
    case all_references(Attrs, Tab, Obj, delete) of
	[] ->
	    ok;
	Refs ->
	    inspect_references(Refs, Tab, Obj, delete)
    end.
%     verify_delete_object1(Attrs, Tab, Obj).

% verify_delete_object1([Attr|Attrs], Tab, Obj) ->
%     check_references(Tab, Attr, Obj, delete),
%     verify_delete_object1(Attrs, Tab, Obj);
% verify_delete_object1([], _, _) ->
%     ok.



verify_record(Tab, Rec, Why) ->
    do_verify(attributes(Tab), Rec, Tab, Why),
    callback_verify(Rec, Why).



do_verify([Attr|As], Rec, Tab, Why) ->
    Val = attribute_value(Tab, Attr, Rec),
    verify_attribute(Val, {Tab, Attr}, Why),
    maybe_check_references(Tab, Attr, Val, Rec, Why),
    do_verify(As, Rec, Tab, Why);
do_verify([], _, _, _) ->
    ok.

maybe_check_references(Tab, Attr, Val, Rec, scan) -> ok;
maybe_check_references(Tab, Attr, Val, Rec, write) ->
    check_references(Tab, Attr, Val, Rec, write).

%% check that a specific attribute is of the right type etc.
%% if NULL, verify that it's not a required attribute.
%%
verify_attribute(Val, Attr) ->
    verify_attribute(Val, Attr, write).


verify_attribute(Val, Attr, scan) ->
    if Val == ?NULL -> ok;
       true ->
	    Type = type(Attr),
	    verify_type(Val, Type, Attr)
    end;
verify_attribute(?NULL, Attr, Why) ->
    case required(Attr) of
	true -> 
	    exit({required_attribute, Attr});
	_ ->
	    ok
    end;
verify_attribute(Val, Attr, Why) ->
    Type = type(Attr),
    ?dbg("Type of ~p (~p) defined as ~p~n", [Attr, Val, Type]),
    verify_type(Val, Type, Attr),
    verify_bounds(Val, Attr).



%% callback_verify(Record, Why)
%% This allows a user to register his own verification trigger for a certain
%% record. This trigger function should call mnesia:abort() if verification
%% fails.
%%
callback_verify(Rec, scan) ->
    ok;
callback_verify(Rec, Why) ->
    Tab = element(1, Rec),
    case table_property(Tab, verify) of
	undefined ->
	    ok;
	F ->
	    F(Rec)
    end.

%% verify_type(Value, TypeInfo, AttrInfo)
%%
%% AttrInfo is for a more descriptive abort reason.
%%
verify_type(?NULL, Attr) -> ok;
verify_type(Val, Attr) ->
    verify_type(Val, type(Attr), Attr).

    

verify_type(X, {record, Rec}, _) when tuple(X) ->
    verify_write(Rec, X);
verify_type(X, {compound, _}, _) -> 
    %% compound attributes are normally not included in the attributes() return
    %% value (This whole issue needs more work), so we shouldn't arrive here...
    ok;  % verify components individually
verify_type(X, {tuples,Arity,KeyPos}, Attr) ->
    verify_tuples_type(X, Arity, {{tuples,Arity,KeyPos}, Attr});
verify_type(X, term, _) -> ok;
verify_type(X, list, _) when list(X) -> ok;
verify_type(X, tuple, _) when tuple(X) -> ok;
verify_type(X, atom, _) when atom(X) -> ok;
verify_type(X, string, Attr) ->
    verify_string(X, Attr);
verify_type(X, text, Attr) ->
    if atom(X) -> ok;
       list(X) -> 
	    verify_string(X, Attr);
       true ->
	    violation(type, {X, text, Attr})
    end;
verify_type(X, number, _) when integer(X) -> ok;
verify_type(X, number, _) when float(X) -> ok;
verify_type(X, integer, _) when integer(X) -> ok;
verify_type(X, float, _) when float(X) -> ok;
verify_type({Node, {MS,S,US}}, oid, _) 
when atom(Node), integer(MS), integer(S), integer(US) ->
    ?dbg("valid oid~n", []),
    ok;
verify_type(X, any, _) -> ok;
verify_type(X, undefined, Attr) ->
    ?dbg("Type information missing for ~p.~n", [Attr]),
    ok;
verify_type(X, {apply, M,F}, _) -> M:F(X);
verify_type(X, F, _) when function(F) -> F(X);
verify_type(X, Other, Attr) ->
    violation(type, {X, Other, Attr}).


%% verify_bounds(Value, TypeInfo, AttrInfo)
%%
%% Checks boundary conditions:
%%   {inclusive, Min, Max} |
%%   {exclusive, Min, Max} |
%%   F(Value) when function(F)
%%
%% AttrInfo is for a more descriptive abort reason.
%%

verify_bounds(Val, Attr) ->
    verify_bounds(Val, bounds(Attr), Attr).


verify_bounds(X, {inclusive, {Min, Max}}, _) when X >= Min, X =< Max ->
    ok;
verify_bounds(X, {exclusive, {Min, Max}}, _) when X > Min, X < Max ->
    ok;
verify_bounds(X, undefined, _) ->
    ok;
verify_bounds(X, F, _) when function(F) -> 
    F(X);
verify_bounds(X, Other, Attr) ->
    violation(bounds, {X, Other, Attr}).


%% verify_string(List, AttrInfo)
%%
%% Checks that List contains only printable characters.
%% (this means that list_to_binary(List) will work.
%%
%% AttrInfo is for a more descriptive abort reason.
%%
verify_string(List, Attr) ->
    case catch list_to_binary(List) of
	{'EXIT', _} ->
	    violation(type, {List, string, Attr});
	_ -> ok
    end.

verify_tuples_type([H|T], Arity, AttrInfo) when tuple(H) ->
    if size(H) == Arity ->
	    verify_tuples_type(T, Arity, AttrInfo);
       true ->
	    violation(arity, {H, AttrInfo})
    end;
verify_tuples_type([], _, _) -> ok;
verify_tuples_type([H|_], Arity, AttrInfo) ->
    violation(type, {H, AttrInfo}).


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
%%		no_action | cascade | set_default | set_null | return
%%
references(Attr) ->
    attr_property(Attr, references).


% check_references(Tab, Attr, Obj, Context) ->
%     case references({Tab, Attr}) of
% 	undefined ->
% 	    [];
% 	Refs ->
% 	    Val = attribute_value(Tab, Attr, Obj),
% 	    follow_refs(Refs, Val, Context, Tab, Attr)
%     end.

all_references(Attrs, Tab, Obj, Context) ->
    lists:foldr(
      fun(Attr, Acc) ->
	      case references({Tab, Attr}) of
		  undefined ->
		      Acc;
		  Refs ->
		      Val = attribute_value(Tab, Attr, Obj),
		      [{Attr, Val, Refs}|Acc]
	      end
      end, [], Attrs).

%% in case we've already fetched the value.
check_references(Tab, Attr, Val, Obj, Context) ->
    case references({Tab, Attr}) of
	undefined ->
	    [];
	Refs ->
	    ?dbg("follow refs for ~p (~p)~n", [{Tab,Attr}, Val]),
	    follow_refs(Refs, Val, Context, Tab, Attr)
    end.

%% If the related Action says 'ignore', do not follow the reference.
%%
follow_refs([{Tab2, Attr2, {_,ignore,_}}|Refs], Val, delete, Tab1, Attr1) ->
    follow_refs(Refs, Val, delete, Tab1, Attr1);
follow_refs([{Tab2, Attr2, {_,_,ignore}}|Refs], Val, write, Tab1, Attr1) ->
    follow_refs(Refs, Val, write, Tab1, Attr1);
follow_refs([{Tab2, Attr2, Actions}|Refs], Val, Context, Tab1, Attr1) ->
    {Match, _, _} = Actions,
    Objs = ref_match(Tab2, Attr2, Match, Val, Tab1, Attr1),
    Objs1 = perform_ref_actions(Actions, Objs, Context, Tab2, Attr2, Val),
    Objs1 ++ follow_refs(Refs, Val, Context, Tab1, Attr1);
follow_refs([], _, _, _, _) ->
    [].


inspect_references(Refs, Tab, Obj, delete) ->
    %% to avoid a possible endless loop, delete Obj first
    do_delete_object(Tab, Obj),
    lists:foldl(
      fun({Attr,Val, Refs1}, Acc) ->
	      follow_refs(Refs1, Val, delete, Tab, Attr) ++ Acc
      end, [], Refs).


perform_ref_actions(_, Objs, read, _, _, _) -> Objs;
perform_ref_actions(Actions, Objs, delete, Tab2, Attr2, Val) ->
    {_Match, OnDelete, _OnUpdate} = Actions,
    case OnDelete of
	cascade ->
	    [delete_object(Tab2, Obj) || Obj <- Objs],
	    Objs;
	set_null ->
	    cascade_update(Objs, Tab2, Attr2, ?NULL);
	set_default ->
	    cascade_update(Objs, Tab2, Attr2, default({Tab2, Attr2}));
	return ->
	    %% This means we only return related objects - don't touch
	    Objs;
	no_action ->
	    if Objs == [] ->
		    [];
	       true ->
		    exit({ref_integrity, {delete, [Tab2,Attr2,Val]}})
	    end
    end;
perform_ref_actions(Actions, Objs, write, Tab2, Attr2, Val) ->
    {_Match, _OnDelete, OnUpdate} = Actions,
    case OnUpdate of
	cascade ->
	    cascade_update(Objs, Tab2, Attr2, Val);
	set_null ->
	    cascade_update(Objs, Tab2, Attr2, ?NULL);
	set_default ->
	    cascade_update(Objs, Tab2, Attr2, default({Tab2, Attr2}));
	return ->
	    Objs;
	no_action ->
	    %% here we should probably also use the MATCH condition...
	    %% but since MATCH only matters for composite values, we skip it
	    %% for now.
	    if Objs == [] ->
		    exit({ref_integrity, {write, [Tab2, Attr2, Val]}});
	       true ->
		    Objs
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
ref_match(Tab2, Attr2, Match, Val, Tab1, Attr1) when atom(Attr2) ->
    fetch_objects(Tab2, Attr2, Val);
ref_match(Tab2, Attr2, Match, Val,
	  Tab1, Attr1) when tuple(Attr2), size(Attr2) == size(Val) ->
    Wild = table_info(Tab2, wild_pattern),
    Objs = match_object(Tab2, Wild),
    AttrL = [attribute_position(Tab2, A) || A <- tuple_to_list(Attr2)],
    ValL = tuple_to_list(Val),
    case {has_nulls(ValL), Match} of
	{all, _} ->     Objs;
	{some, full} -> [];
	{some, []} ->   Objs;
	_ ->       ref_compound_match(Objs, AttrL, ValL)
    end;
ref_match(Tab2, AttrF, _, Val, _, _) when function(AttrF) ->
    Wild = table_info(Tab2, wild_pattern),
    Objs = match_object(Tab2, Wild),
    ref_fun_match(Objs, AttrF, Val).


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
ref_fun_match([Obj|Objs], F, Value) ->
    case F(Obj, Value) of
	true -> [Obj|ref_fun_match(Objs, F, Value)];	    
	false -> ref_fun_match(Objs, F, Value)
    end;
ref_fun_match([], _, _) -> [].



%% has_nulls([Value]) -> none | all | some
%% This function tells whether a list of values contains nulls.
%%
has_nulls([?NULL|Vals]) -> has_nulls(Vals, all);
has_nulls([_|Vals]) -> has_nulls(Vals, none);
has_nulls([]) -> all.

has_nulls([?NULL|T], all) -> has_nulls(T, all);
has_nulls([?NULL|T], none) -> has_nulls(T, some);
has_nulls([_|T], all) -> has_nulls(T, some);
has_nulls([_|T], Acc) -> has_nulls(T, Acc);
has_nulls([], Acc) -> Acc.


%% cascade_update(...)
%%
%% Used in connection with referential integrity checks
%% set_default | set_null | (cascading update)
cascade_update(Objs, Tab, Attr, Value) ->
    Attrs = table_info(Tab, attributes),
    Pos = pos(Attrs, Attr, 2, Tab),
    [write(Tab, setelement(Pos, Obj, Value)) || Obj <- Objs].
    

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
make_object(Tab) ->
    make_object(Tab, []).

make_object(Tab, Values) when atom(Tab) ->
    AttrNames = attributes(Tab),
    Attrs = [{N, type({Tab,N}), default({Tab,N})} || N <- AttrNames],
    replace_values(Values, #rdbms_obj{name = Tab, attributes = Attrs});
make_object(Data, Values) ->
    case is_rdbms_obj(Data) of
	true -> replace_values(Values, Data);  % no need to convert
	false ->
	    replace_values(Values, record_to_rdbms_obj(Data))
    end.

make_simple_form(Tab) ->
    make_simple_form(Tab, []).

make_simple_form(Tab, Values) ->
    AttrNames = attributes(Tab),
    Content = lists:foldr(
		fun(N, Acc) ->
			[{N, describe_attribute(Tab, N, Values), []}|Acc]
		end, [], AttrNames),
    {Tab, [], Content}.

describe_attribute(Tab, Attr, Values) ->
    Id = {Tab, Attr},
    Default = default(Id),
    [{value, get_opt(Attr, Values, Default)},
     {type, type(Id)},
     {key_type, key_type(Id)},
     {required, required(Id)},
     {bounds, bounds(Id)},
     {default, default(Id)}].
    
		      
%%% ==========================================================
%%% make_record(atom() | record() | rdbms_obj()) -> record()
%%%
%%% For all attributes where a value is not provided, specified defaults
%%% will be inserted.
%%% ==========================================================
make_record(Name) when atom(Name) ->
    default_record(Name);
make_record(Data) ->
    case is_record(Data) of
	true -> Data;
	false ->
	    rdbms_obj_to_record(Data)
    end.

is_rdbms_obj(X) when record(X, rdbms_obj) -> 
    %% At this point, we verify that the struct is valid. If it's not, 
    %% we EXIT - because it is most likely meant to be a valid struct.
    ok = verify_type(X#rdbms_obj.attributes, {tuples, 3, 1}, 
		     {rdbms_obj, attributes}),
    true;
is_rdbms_obj(_) -> false.

is_record(X) when record(X, rdbms_obj) ->
    %% well, this is technically a record, but not the kind we're after
    false;
is_record(X) when tuple(X) ->
    Tab = element(1, X),
    ok = verify_record(Tab, X, scan),
    true.
    
record_to_rdbms_obj(X) ->
    Tab = element(1, X),
    AttrNames = attributes(Tab),
    Attrs = [{N, type({Tab,N}), attribute_value(Tab, N, X)} ||
		N <- AttrNames],
    #rdbms_obj{name = Tab, attributes = Attrs}.

rdbms_obj_to_record(X) ->
    Tab = X#rdbms_obj.name,
    AttrNames = attributes(Tab),
    Vals = match_attributes(AttrNames, X#rdbms_obj.attributes, Tab),
    list_to_tuple([Tab|Vals]).

match_attributes([H|T], Attrs, Tab) ->
    case lists:keysearch(H, 1, Attrs) of
	{value, {_,_,V}} -> 
	    [V|match_attributes(T, Attrs, Tab)];
	false ->
	    [default({Tab,H})|match_attributes(T, Attrs, Tab)]
    end;
match_attributes([], _, _) ->
    [].

replace_values([{Key, Val}|T], Obj) ->
    Attrs = replace_val(Key, Obj#rdbms_obj.attributes, Val),
    replace_values(T, Obj#rdbms_obj{attributes = Attrs});
replace_values([], Obj) ->
    Obj.

replace_val(Key, [{Key, Type, _OldVal}|T], Val) ->
    [{Key, Type, Val}|T];
replace_val(Key, [H|T], Val) ->
    [H|replace_val(Key, T, Val)].
    
%% attribute_value(Tab, Attr, Object)
%%
%% This is used to figure out the value of an attribute.
%% Needed since the attribute could be compound.
attribute_value(Tab, Attr, Object) ->
    case type({Tab, Attr}) of
	{compound, SubAttrs} ->
	    [element(attribute_position(Tab, A), Object) || A <- SubAttrs];
	_ ->
	    element(attribute_position(Tab, Attr), Object)
    end.


%%
%% type ::= atom | list | tuple | string | number | integer | float |
%%          any | term | record
%%          
type(Attr) ->
    attr_property(Attr, type).


access(Type, Attr) -> undefined.

%%
%% required ::= true | false
%% 
required(Attr) ->
    case type(Attr) of
	oid ->
	    %% We enforce required==true for attributes of type oid.
	    true;
	_ ->
	    case attr_property(Attr, required) of
		undefined ->
		    false;
		Other ->
		    Other
	    end
    end.

%%
%% default ::= Value
%% 
default(Attr) ->
    case attr_property(Attr, default) of
	undefined ->
	    case type(Attr) of
		{record, Rec} ->
		    default_record(Rec);
		oid ->
		    {node(), erlang:now()};
		_ ->
		    ?NULL
	    end;
	Val -> Val
    end.

default_record(Tab) ->
    Defs = [default({Tab, Attr}) || Attr <- attributes(Tab)],
    list_to_tuple([Tab|Defs]).

%% position(Attribute, Record) -> integer()
%% This calculates the element position of a specified attribute
%% Note that compound attributes have no position
%% position(CompoundAttr) will result in exit({invalid_attribute, ...})
%%
position(Attr, Rec) ->
    pos(attributes(Rec), Attr, 1, Rec).

pos([Attr|_], Attr, N, Tab) ->
    N;
pos([_|T], Attr, N, Tab) ->
    pos(T, Attr, N+1, Tab);
pos([], Attr, _, Tab) ->
    exit({invalid_attribute, {Tab, Attr}}).

%% attribute(Position, Record)
attribute(Pos, Rec) ->
    lists:nth(Pos, attributes(Rec)).
%%
%% bounds ::= {Mode, Min, Max}, Mode ::= inclusive | exclusive
%% 
bounds(Attr) ->
    attr_property(Attr, bounds).

%%
%% attrs ::= [attr]
%% -- list of attrs in correct order
%% 
attributes(Tab) ->
    case catch table_info(Tab, attributes) of
	{'EXIT', _} ->
	    global_property({record, Tab, attributes});
	Attrs when list(Attrs) ->
	    Attrs
    end.

%% all_attributes(Tab)
%%    {PhysicalAttributes, LogicalAttributes}
%% where
%%    LogicalAttributes ::= [{Attr, compound, [SubAttr]}]
%%
all_attributes(Tab) ->
    PhysicalAttrs = attributes(Tab),
    case catch table_info(Tab, user_properties) of
	{'EXIT', _} ->
	    {PhysicalAttrs, []};
	Props ->
	    CompoundAttrs = 
		[{A, {compound, Sub}} || 
		    {{attr,A,type},{compound,Sub}} <- Props],
	    {PhysicalAttrs, CompoundAttrs}
    end.




attribute_position(Tab, Attr) ->
    pos(attributes(Tab), Attr, 2, Tab).



action_on_delete(Rec) ->
    case table_property(Rec, action_on_delete) of
	undefined ->
	    default;
	Other ->
	    Other
    end.
action_on_read(Rec) ->
    case table_property(Rec, action_on_read) of
	undefined ->
	    default;
	Other ->
	    Other
    end.
action_on_write(Rec) ->
    case table_property(Rec, action_on_write) of
	undefined ->
	    default;
	Other ->
	    Other
    end.


%% key_type(Tab, Attribute)
%% This checks whether Attribute of Tab is a key, and if so, which type of key
%% Possible return values are:
%% - primary    - the primary (unique) key
%% - secondary  - non-unique key (has an index attached to it)
%% - {compound, Sub} - compound attribute (may contain keys)
%% - attribute  - not a key
key_type({Tab, Attr}) ->
    key_type(Tab, Attr).

key_type(Tab, Attr) ->
    case attr_property({Tab, Attr}, key_type) of
	undefined ->
	    case type({Tab, Attr}) of
		{compound, SubAttrs} ->
		    {compound, SubAttrs};
		_ ->
		    Apos = attribute_position(Tab, Attr),
		    case (Apos == ?KEYPOS) of
			true -> primary;
			false -> 
			    case table_info(Tab, index) of
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


attr_property({Tab, Attr}, Prop) ->
    case catch mnesia:read_table_property(Tab, {attr, Attr, Prop}) of
	{'EXIT', _} ->
	    global_property({attr, Attr, Prop});
	{_, Value} -> Value
    end;
attr_property(Attr, Prop) ->
    global_property({attr, Attr, Prop}).


table_property(Tab, Prop) ->
    case catch mnesia:read_table_property(Tab, {tab, Tab, Prop}) of
	{'EXIT', _} -> undefined;
	{_, Value} -> Value
    end.

global_property(Prop) ->
    case catch mnesia:read_table_property(schema, Prop) of
	{'EXIT', _} -> undefined;
	{_, Value} -> Value
    end.
		    

set_property(Key, Value) ->
    F = fun() ->
		do_set_property(Key, Value)
	end,
    mnesia_schema:schema_transaction(F).

do_set_property({Class, Key, Type}, Value) ->
    {Tab, LookupKey} = val_key({Class, Key, Type}),
    ?dbg("write_table_prop(~p, ~p)~n", [Tab, {LookupKey, Value}]),
    write_property(Tab, {LookupKey, Value});
do_set_property({Class, Key}, Opts) when list(Opts) ->
    lists:foreach(
      fun({Type, Value}) ->
	      do_set_property({Class, Key, Type}, Value)
      end, Opts).


val_key({attr, {Tab, Attr}, Type}) ->	  {Tab, {attr, Attr, Type}};
val_key({attr, Attr, Type}) ->		  {schema, {attr, Attr, Type}};
val_key({record, Rec, Type}) ->           {schema, {record, Rec, Type}};
val_key({tab, Tab, Type}) ->              {Tab, {tab, Type}}.


%% verify_table_properties(Properties, TableName) -> [Property]
%%
%% This function returns a list of properties (supposedly verified)
%% that is meant to be specified in a create_table(TableName, Options) call.
%% 
%% Currently, not that much verification takes place here; we make an attempt
%% to verify the referential integrity details. EXIT if we see something odd.
%%
verify_table_properties([{{attr, Attr, references}, Refs}|Props], Tab) ->
    Refs1 = check_ref_props(Tab, Attr, Refs),
    [{{attr, Attr, references}, Refs1}|verify_table_properties(Props, Tab)];
verify_table_properties([{{attr, {Tab,Attr}, Type}, Value}|Props], Tab) ->
    [{{attr, Attr, Type}, Value}|verify_table_properties(Props, Tab)];
verify_table_properties(
  [{{attr, Attr, Type}, Value}|Props], Tab) when atom(Attr)->
    [{{attr, {Tab, Attr}, Type}, Value}|verify_table_properties(Props, Tab)];
verify_table_properties([{{tab, Tab, Type}, Value}|Props], Tab) ->
    [{{tab, Type}, Value}|verify_table_properties(Props, Tab)];
verify_table_properties([], _) ->
    [].


write_property(Tab, {{attr, Attr, drop_references}, Refs}) ->
    case references({Tab,Attr}) of
	[] ->
	    ok;
	OldRefs ->
	    case [{T,A,RA} || {T,A,RA} <- OldRefs,
			      {T1,A1} <- Refs,
			      {T,A} == {T1,A1}] of
		[] ->
		    ok;
		DelRefs ->
		    NewRefs = OldRefs -- DelRefs,
		    do_write_property(Tab, {{attr,Attr,references}, NewRefs})
	    end
    end;
write_property(Tab, {{attr, Attr, add_references}, Refs}) ->
    Refs1 = check_ref_props(Tab, Attr, Refs),
    Refs2 = case references({Tab,Attr}) of
		undefined ->
		    Refs1;
		OldRefs ->
		    merge_refs(Refs1, OldRefs)
	    end,
    do_write_property(Tab, {{attr,Attr,references}, Refs2});
write_property(Tab, {{attr, Attr, references}, Refs}) ->
    ?dbg("referential property~n", []),
    Refs1 = check_ref_props(Tab, Attr, Refs),
    do_write_property(Tab, {{attr,Attr,references}, Refs1});
write_property(Tab, Prop) ->
    do_write_property(Tab, Prop).

do_write_property(Tab, Prop) ->
    mnesia_schema:do_write_table_property(Tab, Prop).



merge_refs(R1, R2) ->
    merge_refs(R1, R2, []).

merge_refs([{Tab,Attr,Actions}=R|Refs], Old, Acc) ->
    case [{T,A,RA} || {T,A,RA} <- Old,
		      T == Tab,
		      A == Attr] of
	[] ->
	    merge_refs(Refs, Old, [R|Acc]);
	[R] ->
	    %% duplicate -- not a problem
	    merge_refs(Refs, Old, Acc);
	[{_,_,As}] ->
	    exit({reference_conflict, {Tab,Attr,Actions,As}})
    end;
merge_refs([], Old, Acc) ->
    Old ++ lists:reverse(Acc).
	    

%% Referential integrity rules are stored as metadata in the following way:
%% {attr, {Tab, Attr}, [{Tab2, Attr2, RefActions}]}, where
%% Tab    : the referencing table
%% Attr   : the referencing attribute
%% Tab2   : the referenced table
%% Attr2  : the referenced attribute(s) - 
%%          atom() | {atom()} | function(Object, Value)
%% RefActions : {Match, DeleteAction : Action, UpdateAction : Action}
%% Match  : handling of null values - partial | full
%% Action : referential action - 
%%		no_action | cascade | set_default | set_null | return | ignore

check_ref_props(Tab, Attr, [P|Props]) ->
    [check_ref_prop(Tab, Attr, P)|check_ref_props(Tab, Attr, Props)];
check_ref_props(_, _, []) -> [].

check_ref_prop(Tab, Attr, {Tab2, Attr2, RefActions}) ->
    ?dbg("check_ref_prop(~p, ~p,~p)~n", [Tab,Attr, {Tab2, Attr2, RefActions}]),
    Actions = check_ref_actions(RefActions),
    {Tab2, Attr2, Actions}.

check_ref_actions({Match, DelActions, UpdateActions}) ->
    ?dbg("check_ref_actions(~p)~n", [{Match, DelActions, UpdateActions}]),
    valid_match_option(Match),
    valid_delete_option(DelActions),
    valid_update_option(UpdateActions),
    {Match, DelActions, UpdateActions};
check_ref_actions(RefActions) when list(RefActions) ->
    ?dbg("check_ref_actions(~p)~n", [RefActions]),
    Match = get_opt(match, RefActions, []),
    DelActions = get_opt(delete, RefActions, no_action),
    UpdateActions = get_opt(update, RefActions, no_action),
    valid_match_option(Match),
    valid_delete_option(DelActions),
    valid_update_option(UpdateActions),
    {Match, DelActions, UpdateActions}.

valid_match_option(Match) ->
    valid_option(match, Match, [full, partial, []]).
valid_delete_option(DelActions) ->
    valid_option(delete, DelActions, 
		 [no_action, cascade, set_default, set_null, return, ignore]).
valid_update_option(UpdateActions) ->
    valid_option(update, UpdateActions, 
		 [no_action, cascade, set_default, set_null, return, ignore]).


valid_option(Context, Opt, Valid) ->
    case lists:member(Opt, Valid) of
	true -> ok;
	false ->
	    exit({invalid_option, {Context, Opt}})
    end.

get_opt(Key, [{Key, Val}|_], _) -> Val;
get_opt(Key, [H|T], Default) -> get_opt(Key, T, Default);
get_opt(_, [], Default) -> Default.
    


get_mnesia_transaction_data() ->
    case get(mnesia_activity_state) of
        {_, ActivityId, Opaque} ->
           {ActivityId, Opaque};
        _ -> 
	    mnesia:abort(no_transaction)
    end.

%%%----------------------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%%----------------------------------------------------------------------


