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

%%%-------------------------------------------------------------------
%%% File    : rdbms_groups.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : Allows tables to be grouped in a hierarchy
%%%
%%% Created : 21 Dec 2005 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_groups).

%% functions that start their own (schema) transaction
-export([add_group/1, add_group/2,
	 add_members/2]).

%% functions to be called when inside a schema transaction
-export([do_add_group/2,   % (Group, Members)
	 do_add_members/2,
	 do_drop_members/2,
	 do_drop_membership/1,
	 do_group_info/2]).
	 
-import(lists, [foreach/2]).

%%% Groups
%%% -- A way of grouping tables, recursively in large systems
%%% -- It is not possible to be member of more than one group.

add_group(Group) ->
    add_group(Group, []).

add_group(Group, Members) when Group =/= undefined ->
    mnesia_schema:schema_transaction(
      fun() ->
	      do_add_group(Group, Members)
      end).

do_add_group(Group, Members) ->
    case do_group_info(Group, membership) of
	undefined ->
	    do_check_members(Group, Members),
	    rdbms_props:do_write_property(
	      schema, {{group, Group, membership}, []}),
	    do_add_members(Group, Members);
	_ ->
	    mnesia:abort({group_exists, Group})
    end.

do_check_members(Group, Members) ->
    foreach(
      fun({table, T}) when is_atom(T) ->
	      case mnesia:table_info(T, size) of
		  undefined -> mnesia:abort({unknown_table, T});
		  _ ->
		      case do_membership({table, T}) of
			  undefined -> ok;
			  _ ->
			      mnesia:abort({redefining_member, {table,T}})
		      end
	      end;
	 ({group, G}) when is_atom(G), G =/= Group ->
	      case do_group_info(G, membership) of
		  undefined -> mnesia:abort({unknown_group, G});
		  [] -> ok;
		  [_|_] -> mnesia:abort({redefining_member, {group,G}})
	      end
      end, Members).
	    

add_members(Group, Members) ->
    mnesia_schema:schema_transaction(
      fun() ->
	      do_check_members(Group, Members),
	      do_add_members(Group, Members)
      end).

do_add_members(Group, Members) ->
    foreach(
      fun({table, T}) ->
	      rdbms_props:do_write_property(T, {membership, Group});
	 ({group, G}) ->
	      rdbms_props:do_write_property(
		schema, {{group, G, membership}, Group})
      end, Members),
    rdbms_props:do_write_property(schema, {{group, Group, members}, Members}).

group_info(G, membership) ->
    case catch mnesia:read_table_property({group, G, membership}) of
	{'EXIT', _} -> undefined;
	undefined -> undefined;
	{_, Parent} ->
	    Parent
    end;
group_info(G, members) ->
    case catch mnesia:read_table_property({group, G, members}) of
	{'EXIT', _} ->
	    undefined;
	undefined ->
	    undefined;
	{_, Members} ->
	    Members
    end.


do_group_info(G, membership) ->
    do_membership({group, G});
do_group_info(G, members) ->
    case catch mnesia_schema:do_read_table_property(
		 schema, {group, G, members}) of
	{'EXIT', _} ->
	    undefined;
	{_, Members} when is_list(Members) ->
	    Members
    end.

do_membership({table,T}) ->
    case catch mnesia_schema:do_read_table_property(T, membership) of
	{'EXIT', _} ->
	    undefined;
	undefined ->
	    undefined;
	{_, Parent} ->
	    Parent
    end;
do_membership({group, G}) ->
    case catch mnesia_schema:do_read_table_property(
		 schema, {group, G, membership}) of
	{'EXIT',_} ->
	    undefined;
	undefined ->
	    undefined;
	{_, Parent} ->
	    Parent
    end.

do_drop_membership(T) ->
    case do_membership({table, T}) of
	undefined -> ok;
	[] ->        ok;
	Group ->
	    catch do_drop_members(Group, [{table,T}])
    end.

do_drop_members(Group, DropMembers) ->
    case do_group_info(Group, members) of
	undefined ->
	    mnesia:abort({unknown_group, Group});
	Members ->
	    NewMembers = Members -- DropMembers,
	    foreach(
	      fun({table, T}) ->
		      mnesia:do_delete_table_property(
			T, membership);
		 ({group, G}) ->
		      mnesia:do_delete_table_property(
			schema, {group, G, membership})
	      end, DropMembers),
	    rdbms_props:do_write_property(
	      schema, {{group, Group, members}, NewMembers})
    end.

