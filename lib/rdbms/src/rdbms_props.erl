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
%%% File    : rdbms_props.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : rdbms property handling
%%%
%%% Created : 14 Dec 2005 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_props).

-export([
	 attr_property/3, attr_property/4,
	 table_property/2, table_property/3,
	 global_property/1, global_property/2,
	 global_typedefs/0,
	 attr_references/2,
	 tab_references/1,
	 indexes/1
	]).

%%% metadata queries when inside a schema transaction
-export([
	 schema_table_property/2, schema_table_property/3,
	 schema_attr_property/3, schema_attr_property/4,
	 schema_global_property/1, schema_global_property/2,
	 schema_table_info/2,
	 schema_global_typedefs/0
	]).


%%% attribute metadata accessors
-export([%bounds/2,	% (Tab, Attr)
	 default/2,	% (Tab, Attr)
	 references/2,	% (Tab, Attr)
	 required/2,	% (Tab, Attr)
	 key_type/2,	% (Tab, Attr)
	 access/1,      % (Tab, Attr)
	 type/2]).	% (Tab, Attr)

%%% Table metadata accessors
-export([attributes/1]).

%%% Modifying metadata
-export([set_property/3,
	 do_set_property/3,
%%% 	 set_global_type/3,
%%% 	 do_set_global_type/3,
	 drop_references/1,
	 do_drop_references/1]).

-export([make_rec_type/1,        % (Tab)
	 normalize_type/1,       % (Type)
	 normalize/1,            % ([Type])
	 global_typedefs_used/1,    % (Tab)
	 schema_global_typedefs_used/1, % (Tab)
	 users_of_global_typedef/1  % (GlobalType)
	]).
-export([a_includes_b/2]).

-include("rdbms.hrl").
-import(lists, [foreach/2]).


type(Tab, Attr) ->
    attr_property(Tab, Attr, type).

%%% bounds(Tab, Attr) ->
%%%     attr_property(Tab, Attr, bounds).

default(Tab, Attr) ->
    case attr_property(Tab, Attr, default) of
	undefined ->
	    case attr_property(Tab, Attr, type) of
		oid ->
		    {node(), erlang:now()};
		_ ->
		    ?NULL
	    end;
	{value, Value} ->
	    Value;
	{auto, M, F} ->
	    M:F(Tab, Attr)
    end.

attributes(Tab) ->
    try mnesia:table_info(Tab, attributes)
    catch
	error:_ ->
	    global_property({record, Tab, attributes})
    end.


references(Tab, Attr) ->
    attr_references(Tab, Attr).

required(Tab, Attr) ->
    attr_property(Tab, Attr, required).

key_type(Tab, Attr) ->
    attr_property(Tab, Attr, key_type).

access(Tab) ->
    case table_property(Tab, acl) of
	undefined ->
	    undefined;
	Name when is_atom(Name) ->
	    global_property({acl, Name});
	L when is_list(L) ->
	    L
    end.
    

attr_property(Tab, Attr, Prop) ->
    table_property(Tab, {attr, Attr, Prop}, undefined).

attr_property(Tab, Attr, Prop, Default) ->
    table_property(Tab, {attr, Attr, Prop}, Default).


table_property(Tab, Prop) ->
    table_property(Tab, Prop, undefined).

table_property(Tab, Prop, Default) ->
    case catch mnesia:read_table_property(Tab, Prop) of
	{'EXIT', _} ->
	    Default;
	{_, Value} ->
	    Value
    end.


global_property(Prop) ->
    table_property(schema, Prop, undefined).

global_property(Prop, Default) ->
    table_property(schema, Prop, Default).




%%% schema_xxxx(...) functions
%%% These are intended for use within schema transactions

schema_table_info(Tab) ->
    mnesia_schema:do_read_table_info(Tab).

schema_table_info(Tab, size) ->
    [_|_] = mnesia_schema:do_read_table_info(Tab),  % assertion
    {_, Tid, Ts} = get(mnesia_activity_state),
    try mnesia_frag:table_info(Tid, Ts, Tab, size)
    catch
	error:_ ->
	    %% table exists, but has not yet been created (i.e. visible
	    %% only within the schema transaction as metadata. 'size' is
	    %% a dynamic property (so is 'memory', but why would one want
	    %% to check that within a schema transaction?)
	    0
    end;
schema_table_info(Tab, Item) ->
    Info = mnesia_schema:do_read_table_info(Tab),
    if Item == all -> Info;
       true ->
	    case lists:keysearch(Item, 1, Info) of
		{value, {_, Value}} ->
		    Value;
		false ->
		    mnesia:abort({no_exists,Tab,Item})
	    end
    end.

schema_table_property(Tab, Prop) ->
    schema_table_property(Tab, Prop, undefined).

schema_table_property(Tab, Prop, Default) ->
    Props = schema_table_info(Tab, user_properties),
    case lists:keysearch(Prop, 1, Props) of
	{value, {_, Value}} ->
	    Value;
	false ->
	    Default
    end.

schema_attr_property(Tab, Attr, Prop) ->
    schema_attr_property(Tab, Attr, Prop, undefined).

schema_attr_property(Tab, Attr, Prop, Default) ->
    Props = schema_table_info(Tab, user_properties),
    case lists:keysearch({attr,Attr,Prop}, 1, Props) of
	{value, {_, Value}} ->
	    Value;
	false ->
	    Default
    end.

schema_global_property(Prop) ->
    schema_table_property(schema, Prop, undefined).

schema_global_property(Prop, Default) ->
    schema_table_property(schema, Prop, Default).


%%% schema_global_typedefs() ->
%%%     Props = schema_table_info(schema, user_properties),
%%%     [{T,V} || {{typedef, T}, V} <- Props].


attr_references(Tab, Attr) ->
%%%     case attr_property(Tab, Attr, type) of
%%% 	{global, GlobalType} ->
%%% 	    global_references(GlobalType);
%%% 	_ ->
    case tab_references(Tab) of
	[] ->
	    [];
	[_|_] = AllRefs ->
	    lists:foldr(
	      fun({attr, A, _} = R, Acc) when A==Attr ->
		      [R|Acc];
		 ({attr, As, _} = R, Acc) when is_list(As) ->
		      case lists:member(Attr, As) of
			  true ->
			      [R|Acc];
			  false ->
			      Acc
		      end;
		 (_, Acc) -> Acc
	      end, [], AllRefs)
    end.


%%% global_references(Attr) when is_atom(Attr) ->
%%%     case global_property({attr, Attr, references}) of
%%%  	undefined ->
%%%  	    [];
%%%  	Refs when is_list(Refs) ->
%%%  	    Refs
%%%     end.


indexes(Tab) ->
    table_property(Tab, indexes, []).




%%% set_global_type(Class, Name, Opts) ->
%%%     F = fun() ->
%%% 		do_set_global_type(Class, Name, Opts)
%%% 	end,
%%%     mnesia_schema:schema_transaction(F).

%%% do_set_global_type(acl, Name, Opts) ->
%%%     do_set_property(schema, {acl,Name}, Opts);
%%% do_set_global_type(Class, Name, Opts) ->
%%%     mnesia_schema:verify(true, lists:member(Class, [attr, record, acl]),
%%% 			 {bad_global_type, Class, Name}),
%%%     lists:foreach(
%%%       fun({K,V}) ->
%%% 	      do_set_property(schema, {Class,Name,K}, V)
%%%       end, Opts).
    

set_property(Tab, Key, Value) ->
    F = fun() ->
 		do_set_property(Tab, Key, Value)
 	end,
    mnesia_schema:schema_transaction(F).

do_set_property(Tab, Key, Value) when Tab =/= schema ->
    case Key of
	references ->
	    Refs = check_ref_props(Tab, Value),
	    do_write_property(Tab, {references, Refs});
	add_references ->
	    OldRefs = schema_table_property(Tab, references, []),
	    AddRefs = check_ref_props(Tab, Value),
	    NewRefs = merge_refs(AddRefs, OldRefs),
	    do_write_property(Tab, {references, NewRefs});
	drop_references ->
	    OldRefs = schema_table_property(Tab, references, []),
	    NewRefs = drop_refs(Value, OldRefs),
	    do_write_property(Tab, {references, NewRefs});
	indexes ->
	    lists:foreach(fun(#index{}) -> true;
			     (Other) -> mnesia:abort({invalid_index_record,
						      [Tab, Other]})
			  end, Value),
	    do_write_property(Tab, {indexes, Value});
	write_filter ->
	    try ets:match_spec_compile(Value) of
		_ ->
		    do_write_property(Tab, {write_filter, Value})
	    catch
		error:_ ->
		    mnesia:abort({bad_filter, [write, Tab, Value]})
	    end;
	read_filter ->
	    try ets:match_spec_compile(Value) of
		_ ->
		    do_write_property(Tab, {read_filter, Value})
	    catch
		error:_ ->
		    mnesia:abort({bad_filter, [read, Tab, Value]})
	    end;
	acl ->
	    Acl = check_acl(Value, [read, write, delete, '_']),
	    do_write_property(Tab, {acl, Acl});
%%% 	verify ->
%%% 	    case Value of
%%% 		{M, F} when is_atom(M), is_atom(F) ->
%%% 		    do_write_property(Tab, {verify, {M,F}});
%%% 		_Other ->
%%% 		    mnesia:abort({invalid_property, [Tab, Key, Value]})
%%% 	    end;
	{typedef, Name} ->
	    Type = check_typedef(Tab, Name, Value),
	    do_write_property(Tab, {{typedef,Name}, Type});
	{attr, Attr, type} ->
	    Type = check_typedef(Tab, Attr, Value),
	    do_write_property(Tab, {{attr, Attr, type}, Type}),
	    RecType = make_rec_type(Tab),
	    io:format(user,"RecType= ~p~n", [RecType]),
	    io:format("rec_type(~p) = ~p~n", [Tab, RecType]),
	    do_write_property(Tab, {rec_type, RecType});
	{attr, Attr, Prop} ->
	    set_attr_prop(Tab, Attr, Prop, Value)
    end;
do_set_property(schema, Key, Value) ->
    case Key of
	{acl, Name} when is_atom(Name) ->
	    Acl = check_acl(Value, [read, write, delete, '_']),
	    do_write_property(schema, {{acl,Name}, Acl});
%%% 	acl ->
%%% 	    Acl = check_acl(Value, [write, delete]),
%%% 	    do_write_property(schema, {acl, Acl});
%%% 	{attr, Attr, Prop} ->
%%% 	    set_attr_prop(schema, Attr, Prop, Value);
	{typedef, Name} ->
	    Type = check_typedef(schema, Name, Value),
	    do_write_property(schema, {{typedef,Name}, Type})
    end.

check_typedef(Tab, Name, Def) ->
    Props = schema_table_info(Tab, user_properties),
    GlobalProps = if Tab == schema -> [];
		     true -> schema_table_info(schema, user_properties)
		  end,
    Fail = fun() ->
		   mnesia:abort({bad_typedef, [Tab, Name]})
	   end,
    check_type(Def, Props, GlobalProps, Fail),
    %% currently no optimizations
    Def.


check_type(Def, Props, GlobalProps, Fail) ->
    Check = fun(D) ->
		    check_type(D, Props, GlobalProps, Fail)
	    end,
    case Def of
	undefined -> true;
	{tuple, Arity, Ts} when is_integer(Arity), Arity > 0 ->
	    lists:foreach(Check, Ts);
	{tuple, Arity} when is_integer(Arity), Arity > 0 ->
	    true;
	{function, Arity} when is_integer(Arity), Arity > 0 ->
	    true;
	{list, T} -> Check(T);
	{type, T} ->
	    case lists:keymember({typedef,T}, Props) orelse
		lists:keymember({typedef, T}, GlobalProps) of
		true -> true;
		false -> Fail()
	    end;
	{'and', Ts} when is_list(Ts) -> lists:foreach(Check, Ts);
	{'or', Ts} when is_list(Ts) -> lists:foreach(Check, Ts);
	{enum, Vs} when is_list(Vs) -> true;
%%% 	{'andalso', Ts} when is_list(Ts) -> lists:foreach(Check, Ts);
%%% 	{'orelse', Ts} when is_list(Ts) -> lists:foreach(Check, Ts);
	{'not', A} -> Check(A);
	{'<', _} -> true;
	{'>', _} -> true;
	{'==', _} -> true;
	{'=/=', _} -> true;
	{'>=', _} -> true;
	{'=<', _} -> true;
	Bool when is_boolean(Bool) -> true;	
	Simple when is_atom(Simple) ->
	    case lists:member(Simple, simple_builtin_types()) of
		true ->
		    true;
		false ->
		    Fail()
	    end
    end.
 

set_attr_prop(Tab, Attr, Prop, Value) ->
    valid_attr(Tab, Attr),
    Invalid = fun() ->
		      mnesia:abort(
			{invalid_attr_property, [Tab,Attr,Prop,Value]})
	      end,
    case Prop of
%%%	type ->    valid_attr_type(Tab, Attr, Value);
%%%	bounds ->  valid_bounds(Tab, Attr, Value);
	required when is_boolean(Value) -> ok;
	key_type when Value==primary;
		      Value==secondary;
		      Value==none -> ok;
	default ->
	    case Value of 
		{value, _Term} -> ok;
		{auto, {M, F}} when is_atom(M), is_atom(F) -> ok;
		_ ->
		  Invalid()  
	    end;
	_ ->
	    Invalid()
    end,
    do_write_property(Tab, {{attr,Attr,Prop}, Value}),
    case Tab of
	schema ->
	    if Prop == type ->
		    re_normalize(Attr);
%%% 	       Prop == bounds ->
%%% 		    Tabs = users_of_global_type(Attr),
%%% 		    lists:foreach(
%%% 		      fun(Tab1) ->
%%% 			      BR = make_bounds_rec(Tab1),
%%% 			      do_write_property(Tab1, {bounds_rec, BR})
%%% 		      end, Tabs);
	       true ->
		    ok
	    end;
	_ ->
	    ok
    end.
	



%%% valid_attribute(Tab, Attr) ->
%%%     Info = mnesia_schema:do_read_table_info(Tab),
%%%     {value, {_,Attrs}} = lists:keysearch(attributes, 1, Info),
%%%     case lists:member(Attr, Attrs) of
%%% 	false ->
%%% 	    mnesia:abort({invalid_attribute, {Tab, Attr}});
%%% 	true ->
%%% 	    true
%%%     end.



%%% valid_attr_type(Tab, Attr, Type) ->
%%%     Invalid = fun() ->
%%% 		      mnesia:abort({invalid_attribute_type, [Tab,Attr,Type]})
%%% 	      end,
%%%     case Type of
%%% 	{alt, AltTypes} ->
%%% 	    valid_elem_types(Tab, [Attr, {alt, AltTypes}], AltTypes);
%%% 	{tuple, Arity, ElemTypes} 
%%% 	when is_integer(Arity), Arity > 0, length(ElemTypes)==Arity ->
%%% 	    valid_elem_types(Tab, [Attr, {tuple, ElemTypes}], ElemTypes);
%%% 	{tuple, Arity} when is_integer(Arity), Arity > 0 ->
%%% 	    true;
%%% 	{function, Arity} when is_integer(Arity), Arity > 0 ->
%%% 	    true;
%%% 	{list, ElemTypes} ->
%%% 	    valid_elem_types(Tab, [Attr, {list, ElemTypes}], ElemTypes);
%%% 	{global, GlobalType} ->
%%% 	    case lists:member(GlobalType, schema_global_typedefs()) of
%%% 		true -> true;
%%% 		false ->
%%% 		    Invalid()
%%% 	    end;
%%% 	{const, _Value} ->
%%% 	    true;
%%% 	{function, Arity} when is_integer(Arity), Arity >= 0 ->
%%% 	    true;
%%% 	_ ->
%%% 	    case lists:member(Type, simple_builtin_types()) of
%%% 		true ->
%%% 		    true;
%%% 		false ->
%%% 		    Invalid()
%%% 	    end
%%%     end.

simple_builtin_types() ->
    [any, undefined, atom, integer, float, number, string, text, list,
     nil, tuple, pid, port, reference, binary, oid, function].

%%% valid_elem_types(Tab, ParentType, Types) ->
%%%     lists:foreach(
%%%       fun(T) ->
%%% 	      valid_attr_type(Tab, ParentType ++ [T], T)
%%%       end, Types).


re_normalize(GlobalType) ->
    lists:foreach(
      fun(Tab) ->
	      TypeRec = make_rec_type(Tab),
	      do_write_property(Tab, {rec_type, TypeRec})
      end, users_of_global_typedef(GlobalType)).

global_typedefs() ->
    global_typedefs(mnesia:table_info(schema, user_properties)).
schema_global_typedefs() ->
    global_typedefs(schema_table_info(schema, user_properties)).

global_typedefs(Props) when is_list(Props) ->
    [{Name, Type} || {typedef, Name, Type} <- Props].


global_typedefs_used(Tab) when Tab =/= schema ->
    global_typedefs_used(mnesia:table_info(Tab, all), global_typedefs()).
schema_global_typedefs_used(Tab) when Tab =/= schema ->
    global_typedefs_used(schema_table_info(Tab, all), schema_global_typedefs()).

global_typedefs_used(TI, Globals) ->
    Attrs = proplists:get_value(attributes, TI),
    TP = proplists:get_value(user_properties, TI),
    case lists:foldl(
	   fun(Attr, Acc) ->
		   Type = proplists:get_value({attr,Attr,type}, TP),
		   uses_global_typedef(Type, Acc)
	   end, ordsets:new(), Attrs) of
	[] ->
	    %% probably a common case
	    [];
	Ts ->
	    Dict = dict:from_list(Globals),
	    lists:foldr(
	      fun(T, Acc) ->
		      case dict:find(T, Dict) of
			  {ok, Def} ->
			      [{T,Def}|Acc];
			  error ->
			      Acc
		      end
	      end, [], Ts)
    end.

uses_global_typedef({type, T}, Acc) ->				
    ordsets:add_element(T,Acc);
uses_global_typedef({Tag, Ts}, Acc) when Tag==list; Tag=='and'; Tag=='or' ->
    lists:foldl(
      fun(T, Acc1) ->
	      uses_global_typedef(T, Acc1)
      end, Acc, Ts);
uses_global_typedef({tuple, _, Ts}, Acc) ->
    lists:foldl(
      fun(T, Acc1) ->
	      uses_global_typedef(T, Acc1)
      end, Acc, Ts);
uses_global_typedef(_, Acc) ->
    Acc.


users_of_global_typedef(GlobalType) ->
    lists:filter(
      fun(Tab) ->
	      lists:member(GlobalType, global_typedefs_used(Tab))
      end,
      mnesia:system_info(tables) -- [schema]).  % TODO: schema_ version




%%% make_bounds_rec(Tab) when Tab =/= schema ->
%%%     Attrs = schema_table_info(Tab, attributes),
%%%     list_to_tuple([bounds | [schema_attr_property(Tab, Attr, bounds) || 
%%% 				Attr <- Attrs]]).

make_rec_type(Tab) when Tab =/= schema ->
    Info = schema_table_info(Tab),
    Attrs = proplists:get_value(attributes, Info),
    RecName = proplists:get_value(record_name, Info),
    TabProps = proplists:get_value(user_properties, Info),
    TabTypeDefs = [{N,D} || {{typedef,N},D} <- TabProps],
    GlobTypeDefs = [{N,D} || {{typedef,N},D} <-
				 schema_table_info(schema, user_properties)],
    TypeDefs = TabTypeDefs ++ GlobTypeDefs,
    {tuple, length(Attrs)+1,
     [{'==', RecName}|
      [normalize_type(type_of(Tab, Attr, TabProps, TypeDefs)) ||
	  Attr <- Attrs]]}.

type_of(_Tab, Attr, TP, TD) ->
    type_of(proplists:get_value({attr,Attr,type}, TP, no_type), TD).

type_of({type, T}, TD) ->
    type_of(proplists:get_value(T, TD), TD);
type_of({Op,Ts},TD) when Op=='or';Op=='and' ->
    {Op,[type_of(T,TD) || T <- Ts]};
type_of({'not', T}, TD) ->
    {'not', type_of(T, TD)};
type_of({tuple,A,Ts},TD) ->
    {tuple,A,[type_of(T,TD) || T <- Ts]};
type_of({list, T},TD) ->
    {list, type_of(T,TD)};
type_of(T, _) ->
    T.

normalize_type({Op, Ts}) when Op=='or' ->
    case normalize(Ts) of
	[] ->
	    no_type;
	[T] ->
	    T;
	[_|_] = Ts1 ->
	    {Op, Ts1};
	T ->
	    T
    end;
normalize_type({Op, Ts}) when Op=='and' ->
    case normalize(Ts) of
	[] ->
	    no_type;
	[T] ->
	    T;
	[_|_] = Ts1 ->
	    case lists:member(false, Ts1) of
		true -> false;
		false ->
		    {Op, Ts1}
	    end;
	T ->
	    T
    end;
normalize_type({list, LT}) ->
    case normalize_type(LT) of
	false ->
	    nil;
	undefined ->
	    list;
	LT1 ->
	    {list, LT1}
    end;
normalize_type({tuple, Arity, TTs}) ->
    TTs1 = [normalize_type(T) || T <- TTs],
    case lists:all(fun(any) -> true;
		      (undefined) -> true;
		      (_) -> false
		   end, TTs1) of
	true ->
	    {tuple, Arity};
	false ->
	    {tuple, Arity, TTs1}
    end;
normalize_type(T) ->
    T.

normalize(Ts) ->
    Ts1 = [normalize_type(T) || T <- Ts],
    case prune(Ts1) of
	[] -> any;
	[T] -> T;
	[_|_] = Ts2 ->
	    Ts2
    end.

prune(Ts) -> 
    prune(Ts, []).

prune([T|Ts], Acc) ->
    Acc1 = lists:filter(
	     fun(Tx) when Tx == T ->
		     false;
		(Tx) -> not(a_includes_b(T, Tx))
	     end, Acc),
    prune(Ts, [T|Acc1]);
prune([], Acc) ->
    lists:reverse(Acc).
	 
a_includes_b(_, no_type) -> true;
a_includes_b(any,_) -> true;
a_includes_b(A, B) ->
    case orddict:find(A, ts()) of
	{ok, SubTypes} ->
	    lists:member(t_alias(B), SubTypes);
	error ->
	    false
    end.


%% an orddict
ts() ->
    [{boolean, [true, false]},
     {function,[{function}]},
     {list,[{list},nil]},
     {number,[integer,float]},
     {text,[atom,string,binary]},
     {tuple,[{tuple}]}].

t_alias(T) when is_tuple(T) ->
    {element(1,T)};
t_alias(T) ->
    T.



do_write_property(Tab, Prop) ->
    mnesia_schema:do_write_table_property(Tab, Prop).


check_acl(L, ValidOps) ->
    F = fun({Op, Rhs}, {Good, Bad}) ->
		case lists:member(Op, ValidOps)
		    andalso valid_acl_rhs(Rhs) of
		    true ->
			{[{Op, Rhs}|Good], Bad};
		    false ->
			{Good, [{Op, Rhs}|Bad]}
		end;
	   (Other, {Good, Bad}) ->
		{Good, [Other|Bad]}
	end,
    case lists:foldr(F, {[], []}, L) of
	{Good, []} ->
	    _Sorted = lists:foldr(
			fun(Op, Acc) ->
				case lists:keysearch(Op, 1, Good) of
				    {value, Found} ->
					[Found|Acc];
				    false ->
					Acc
				end
			end, [], ValidOps);
	{_, Bad} ->
	    mnesia:abort({illegal_acl, Bad})
    end.

valid_acl_rhs(Rhs) when is_boolean(Rhs) ->
    true;
valid_acl_rhs({M,F}) when is_atom(M), is_atom(F) ->
    %% assume M:F(Tab, Op, Rec) exists and returns boolean()
    true;
valid_acl_rhs(Other) ->
    mnesia:abort({invalid_acl_rhs, Other}).


%%% valid_bounds(_Tab, _Attr, {inclusive, A, B}) when A =< B ->    true;
%%% valid_bounds(_Tab, _Attr, {exclusive, A, B}) when A =< B ->    true;
%%% valid_bounds(Tab, Attr, Other) ->
%%%     mnesia:abort({invalid_bounds_type, [Tab,Attr,Other]}).

%%% The Acl list is read from top to bottom, so the first 
%%% matching entry is accepted. To simplify the code generation,
%%% we remove redundant entries. We could also flag an error if 
%%% there are duplicates...
%%% remove_redundant([]) ->
%%%     [];
%%% remove_redundant([{Op,_} = H | T]) ->
%%%     [H|remove_redundant([H1 || {Op1,_}=H1 <- T,
%%% 			       Op1 =/= Op])].



tab_references(Tab) ->
    table_property(Tab, references, []).



drop_references(ToTab) ->
    mnesia_schema:schema_transaction(
      fun() ->
 	      do_drop_references(ToTab)
      end).

do_drop_references(ToTab) ->
    foreach(
      fun(Tab) ->
	      case schema_table_property(Tab, references, []) of
		  [] ->
		      ok;
		  [_|_] = OldRefs ->
		      NewRefs = 
			  lists:foldr(
			    fun({Type,Key,Rs}, Acc) ->
				    case lists:foldr(
					   fun({Tab2,_Attr2,_Actions}, Acc2)
					      when Tab2 == ToTab ->
						   Acc2;
					      (Other, Acc2) ->
						   [Other|Acc2]
					   end, [], Rs) of
					[] ->
					    Acc;
					[_|_] = Rs1 ->
					    [{Type,Key,Rs1}|Acc]
				    end
			    end, [], OldRefs),
		      if NewRefs == OldRefs ->
			      ok;
			 true ->
			      do_write_property(Tab, {references, NewRefs})
		      end
	      end
      %% shouldn't use system_info here - tables could've been added/deleted
      end, mnesia:system_info(tables)).


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


check_ref_props(Tab, Refs) when is_list(Refs) ->
    Attrs = schema_table_info(Tab, attributes),
    lists:map(
      fun({attr, Attr, Rs}) when is_list(Rs) ->
	      valid_attr(Tab, Attr, Attrs),
	      {attr, Attr, [check_ref_prop(R) || R <- Rs]};
	 ({index, Ix, Rs}) when is_list(Rs) ->
	      %% TODO: check that Ix is a valid index
	      case rdbms_index:valid_index(Tab, Ix) of
		  true -> ok;
		  false ->
		      mnesia:abort({invalid_index, [Tab,Ix]})
	      end,
	      {index, Ix, [check_ref_prop(R) || R <- Rs]};
	 ({eval, {_M,_F,_XArgs} = Fn, Rs}) when is_list(Rs) ->
	      {eval, Fn, [check_ref_prop(R) || R <- Rs]};
	 (Other) ->
	      mnesia:abort({bad_references,
			    [Tab, Refs,
			     {unknown_format, Other}]})
      end, Refs);
check_ref_props(Tab, Refs) ->
    mnesia:abort({bad_references, [Tab, Refs,
				   unknown_format]}).


merge_refs(R1, R2) ->
    merge_refs(R1, R2, []).

merge_refs([{Type,Key,Actions}=R | Refs], Old, Acc) ->
    case [{T,K,RA} || {T,K,RA} <- Old,
		      T == Type,
		      K == Key] of
	[] ->
	    merge_refs(Refs, Old, [R|Acc]);
	[R] ->
	    %% duplicate -- not a problem
	    merge_refs(Refs, Old, Acc);
	[{_,_,As}] ->
	    exit({reference_conflict, {Type,Key,Actions,As}})
    end;
merge_refs([], Old, Acc) ->
    Old ++ lists:reverse(Acc).
	    
drop_refs(DropRefs, OldRefs) ->
    [{Type,Key,Rs} || {Type,Key,Rs} <- OldRefs,
		      not(lists:member({Type,Key}, DropRefs))].


%%% check_ref_props(Tab, Attr, [P|Props]) ->
%%%     [check_ref_prop(P)|check_ref_props(Tab, Attr, Props)];
%%% check_ref_props(_, _, []) -> [].

check_ref_prop({Tab2, {via_index, _Ix}=Via, RefActions}) ->
%%%     case rdbms_index:valid_index(Tab2, Ix) of
%%% 	true -> ok;
%%% 	false ->
%%% 	    mnesia:abort({invalid_index, [Tab2, Ix]})
%%%     end,
    Actions = check_ref_actions(RefActions, [full]),
    {Tab2, Via, Actions};
check_ref_prop({Tab2, Attr2, RefActions}) ->
    ?dbg("check_ref_prop(~p)~n", [{Tab2, Attr2, RefActions}]),
%%%    valid_attr(Tab2, Attr2),
    ValidMatches = if is_list(Attr2) -> [full, partial];
		      is_atom(Attr2) -> [full]
		   end,
    Actions = check_ref_actions(RefActions, ValidMatches),
    {Tab2, Attr2, Actions}.

check_ref_actions({Match, DelActions, UpdateActions}, ValidMatches) ->
    ?dbg("check_ref_actions(~p)~n", [{Match, DelActions, UpdateActions}]),
    valid_match_option(Match, ValidMatches),
    valid_delete_option(DelActions),
    valid_update_option(UpdateActions),
    {Match, DelActions, UpdateActions};
check_ref_actions(RefActions, ValidMatches) when list(RefActions) ->
    ?dbg("check_ref_actions(~p)~n", [RefActions]),
    Match = get_opt(match, RefActions, full),
    DelActions = get_opt(delete, RefActions, no_action),
    UpdateActions = get_opt(update, RefActions, no_action),
    valid_match_option(Match, ValidMatches),
    valid_delete_option(DelActions),
    valid_update_option(UpdateActions),
    {Match, DelActions, UpdateActions}.

valid_match_option(Match, Valid) ->
    valid_option(match, Match, Valid).
valid_delete_option(DelActions) ->
    valid_option(delete, DelActions, 
		 [no_action, cascade, set_default, set_null, ignore]).
valid_update_option(UpdateActions) ->
    valid_option(update, UpdateActions, 
		 [no_action, cascade, set_default, set_null, ignore]).


valid_option(Context, Opt, Valid) ->
    case lists:member(Opt, Valid) of
	true -> ok;
	false ->
	    mnesia:abort({invalid_option, {Context, Opt}})
    end.


get_opt(Key, [{Key, Val}|_], _) -> Val;
get_opt(Key, [_H|T], Default) -> get_opt(Key, T, Default);
get_opt(_, [], Default) -> Default.
    

valid_attr(Tab, A) ->
    valid_attr(Tab, A, schema_table_info(Tab, attributes)).
valid_attr(Tab, A, Attrs) ->
    IsValid = 
	fun(A1) ->
		case lists:member(A1, Attrs) of
		    true -> ok;
		    false ->
			mnesia:abort({invalid_attr, [Tab,A1]})
		end
	end,
    if is_list(A) ->
	    lists:foreach(IsValid, A);
       is_atom(A) ->
	    IsValid(A);
       true ->
	    mnesia:abort({invalid_attr, [Tab,A]})
    end.

