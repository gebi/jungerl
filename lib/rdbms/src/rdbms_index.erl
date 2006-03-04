%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Purpose: Handles index functionality in mnesia

-module(rdbms_index).

%%% Called by rdbms
-export([read/7,
	 w_read/7,
	 index_values/4]).

-export([update_index/7,
	 index_record/3
%%%	 pos_index/2
%%% 	 dirty_read/4,
%%% 	 dirty_read2/3,
%%% 	 dirty_read3/4,
%%%	 filter_found_set/3
	]).

-export([valid_index/2,
	 valid_index/3]).

%%% Callback for mnesia table load hook
-export([index_init_fun/2, index_init_fun/3]).
-export([match_object/7,
 	 dirty_match_object/3]).

-export([frag_index_read/7]).   % used for index_read on fragmented tabs

-export([dirty_read/4,
	 dirty_index_first/2, dirty_index_first/3,
	 dirty_index_relop/4,
	 dirty_index_next/3,
	 dirty_index_prev/3,
 	 dirty_index_last/2]).

-export([do_add_indexes/2,
	 do_delete_indexes/1]).


%%% called by rdbms_index_load
-export([attr_pos/3,
	 table_info/3]).

%%% called by dirty_read/4 via rpc:call()
-export([dirty_read2/3,
	 dirty_read3/4]).

-import(mnesia_lib, [verbose/2]).
-import(rdbms_frag, [key_to_frag_name/2]).
-import(proplists, [get_value/2]).
%%%-include_lib("mnesia/src/mnesia.hrl").
-include("mnesia.hrl").

-import(rdbms_props, [table_property/2]).
-include("rdbms.hrl").



read(Tid, Ts, Tab, Key, {_Pos,_Tag}=Index, LockKind, VMod) 
  when is_atom(Tab), Tab =/= schema ->
    #index{table_name = IxTab} = Ix = index_record(Tab, Index, VMod),
    verify_key(Tab, Index, Key, LockKind),  % aborts if something fishy
    Oids =
	case Ix#index.type of
	    ordered ->
		Pat = select_pattern(Ix, Key),
		rdbms:select(Tid, Ts, IxTab, Pat, LockKind, VMod);
	    weighted ->
		%% we support weighted indexes here, but we throw away
		%% the weights
		Pat = select_pattern(Ix, Key),
		[O || {O,_} <- 
			  rdbms:select(Tid, Ts, IxTab, Pat, LockKind, VMod)];
	    T when T==bag; T==set ->
		IxObjs = rdbms:read(Tid, Ts, IxTab, Key, LockKind, VMod),
		[Oid || #ix{oid = Oid} <- IxObjs]
	end,
    FoundSet = [rdbms:read(Tid,Ts,Tab,Oid,LockKind,VMod) || Oid <- Oids],
    case table_info(Tab, setorbag, VMod) of
	bag ->
	    filter_found_set(FoundSet, Ix, Key);
	_ ->
	    lists:append(FoundSet)
    end;
read(_Tid, _Ts, Tab, _Key, _Attr, _LockKind, _Vmod) ->
    mnesia:abort({bad_type, Tab}).

w_read(Tid, Ts, Tab, Key, {_Pos,_Tag}=Index, LockKind, VMod) 
  when is_atom(Tab), Tab =/= schema ->
    #index{table_name = IxTab} = Ix = index_record(Tab, Index, VMod),
    verify_key(Tab, Index, Key, LockKind),  % aborts if something fishy
    Oids =
	case Ix#index.type of
	    ordered ->
		%% introduce dummy weights
		Pat = select_pattern(Ix, Key),
		[{O,1} ||
		    O <- rdbms:select(Tid, Ts, IxTab, Pat, LockKind, VMod)];
	    weighted ->
		Pat = select_pattern(Ix, Key),
		rdbms:select(Tid, Ts, IxTab, Pat, LockKind, VMod);
	    bag ->
		%% introduce dummy weights
		IxObjs = rdbms:read(Tid, Ts, IxTab, Key, LockKind, VMod),
		[{Oid, 1} || #ix{oid = Oid} <- IxObjs]
	end,
    FoundSet = [{rdbms:read(Tid,Ts,Tab,Oid,LockKind,VMod), Wt} ||
		   {Oid, Wt} <- Oids],
    case table_info(Tab, setorbag, VMod) of
	bag ->
	    filter_weighted_set(FoundSet, Ix, Key);
	_ ->
	    lists:append(FoundSet)
    end;
w_read(_Tid, _Ts, Tab, _Key, _Attr, _LockKind, _Vmod) ->
    mnesia:abort({bad_type, Tab}).



table_indexes(Tab, rdbms_verify_jit) ->
    rdbms_verify_jit:indexes(Tab);
table_indexes(Tab, {rdbms_verify, VRec}) ->
    rdbms_verify:indexes(Tab, VRec).

access_module(Tab, rdbms_verify_jit) ->
    rdbms_verify_jit:module(Tab);
access_module(Tab, {rdbms_verify, VRec}) ->
    rdbms_verify:module(Tab, VRec).

table_info(Tab, Item, rdbms_verify_jit) ->
    rdbms_verify_jit:table_info(Tab, Item);
table_info(Tab, Item, {rdbms_verify,VRec}) ->
    rdbms_verify:table_info(Tab, Item, VRec).

update_index(Tid, Ts, Tab, Op, Val, LockKind, VMod) ->
    BaseTab = base_tab(Tab, VMod),
    update_indexes(table_indexes(BaseTab, VMod),
		   Tid, Ts, Tab, Op, Val, LockKind, VMod).

update_indexes([], _, _, _, _, _, _, _) -> ok;
update_indexes([_|_] = Ixes, Tid, Ts, Tab, write, Val, LockKind, VMod) ->
    Oid = element(2, Val),
    case table_info(Tab, setorbag, VMod) of
	set ->
	    AMod = access_module(Tab, VMod),
	    OldVal = AMod:dirty_read(Tab, Oid),
	    ix_write_set(Ixes, Oid, Val, OldVal, Tab, VMod,
			 [VMod, Tid, Ts, LockKind]);
	bag ->
	    ix_write_bag(Ixes, Oid, Val, Tab, VMod,
			 [VMod, Tid, Ts, LockKind])
    end;
update_indexes([_|_] = Ixes, Tid, Ts, Tab, delete, Key, LockKind, VMod) ->
    Old = mnesia:dirty_read(Tab, Key),
    update_index_delete(Ixes, VMod, Tid, Ts, Tab, Old, LockKind);
update_indexes([_|_] = Ixes, Tid,Ts,Tab, delete_object, Val, LockKind, VMod) ->
    update_index_delete(Ixes, VMod, Tid, Ts, Tab, [Val], LockKind);
update_indexes([_|_] = Ixes, Tid, Ts, Tab, fill, Val, LockKind, VMod) ->
    %% used when initializing indexes. Like 'write', but we don't read the
    %% previous value.
    Oid = element(2, Val),
    case table_info(Tab, setorbag, VMod) of
	set ->
	    OldVal = [],
	    ix_write_set(Ixes, Oid, Val, OldVal, Tab, VMod,
			 [VMod, Tid, Ts, LockKind]);
	bag ->
	    ix_write_bag(Ixes, Oid, Val, Tab, VMod,
			 [VMod, Tid, Ts, LockKind])
    end.



update_index_delete(Ixes, VMod, Tid, Ts, Tab, Objs, LockKind) ->
    lists:foreach(
      fun(#index{pos = IxPos, m_f = {M, F}, arg = Arg} = Ix) ->
	      Pos = attr_pos(Tab, IxPos, VMod),
	      lists:foreach(
		fun(Obj) ->
			Oid = element(2, Obj),
			IxValues = index_values(Obj, Pos, M, F, Arg),
			delete_ix_values(
			  IxValues, Oid, Ix, [VMod, Tid, Ts, LockKind])
		end, Objs)
      end, Ixes).




ix_write_set(Ixes, Oid, Val, OldVal, Tab, VMod, Mode) ->
    lists:foreach(
      fun(#index{pos = IxPos, m_f = {M, F}, arg = Arg} = Ix) ->
	      Pos = attr_pos(Tab, IxPos, VMod),
	      OldIxValues = ordsets:from_list(
			      case OldVal of
				  [Old] ->
				      index_values(Old, Pos, M, F, Arg);
				  [] ->
				      []
			      end),
	      NewIxValues = ordsets:from_list(
			      index_values(Val, Pos, M, F, Arg)),
	      DelIxValues = OldIxValues -- NewIxValues,
	      WriteIxValues = NewIxValues -- OldIxValues,
	      delete_ix_values(DelIxValues, Oid, Ix, Mode),
	      check_if_unique(Ix, WriteIxValues, Tab, Oid, VMod, Mode),
	      insert_ix_values(WriteIxValues, Oid, Ix, Mode)
      end, Ixes).

ix_write_bag(Ixes, Oid, Val, Tab, VMod, Mode) ->
    lists:foreach(
      fun(#index{pos = IxPos, m_f = {M,F}, arg = Arg} = Ix) ->
	      Pos = attr_pos(Tab, IxPos, VMod),
	      NewIxValues = index_values(Val, Pos, M, F, Arg),
	      insert_ix_values(NewIxValues, Oid, Ix, Mode)
      end, Ixes).


index_values(Tab, Ix, Obj, VMod) when is_integer(Ix) ->
    case attr_pos(Tab, Ix, VMod) of
	P when P > 1 ->
	    [element(P, Obj)];
	_ ->
	    mnesia:abort({invalid_index, {Tab, Ix}})
    end;
index_values(Tab, {Pos0, Tag} = Ix, Obj, VMod) ->
    Ixes = table_indexes(Tab, VMod),
    Pos = attr_pos(Tab, Pos0, VMod),
    %% TODO: shouldn't we normalize the metadata, rather than
    %% expending runtime cycles de-abstracting it?
    case lists:dropwhile(
	   fun(#index{pos = {_, T}}) when T =/= Tag ->
		      true;
	      (#index{pos = {IxPos, T}}) when T == Tag ->
		   Pos =/= attr_pos(Tab, IxPos, VMod)
	   end, Ixes) of
	[] ->
	    mnesia:abort({invalid_index, {Tab, Ix}});
	[#index{m_f = {M,F}, arg = Arg}|_] ->
	    index_values(Obj, Pos, M, F, Arg)
    end.

index_values(Obj, 1, M, F, Arg) ->
    M:F(Obj, Arg);
index_values(Obj, Pos, M, F, Arg) ->
    M:F(element(Pos, Obj), Arg).

delete_ix_values(Vals, Oid, #index{table_name = IxTab,
				   type = ordered}, Mode) ->
    case Mode of
	dirty ->
	    lists:foreach(
	      fun(Key) ->
		      mnesia:dirty_delete(IxTab, {Key, Oid})
	      end, Vals);
	[VMod, Tid, Ts, LockKind] ->
	    AMod = access_module(IxTab, VMod),
	    lists:foreach(
	      fun(Key) ->
		      AMod:delete(Tid, Ts, IxTab, {Key, Oid}, LockKind)
	      end, Vals)
	end;
delete_ix_values(Vals, Oid, #index{table_name = IxTab,
				   type = weighted}, Mode) ->
    case Mode of
	dirty ->
	    lists:foreach(
	      fun({Key,W}) ->
		      mnesia:dirty_delete(IxTab, {Key, W, Oid})
	      end, Vals);
	[VMod, Tid, Ts, LockKind] ->
	    AMod = access_module(IxTab, VMod),
	    lists:foreach(
	      fun({Key,W}) ->
		      AMod:delete(Tid, Ts, IxTab, {Key, W, Oid}, LockKind)
	      end, Vals)
	end;
delete_ix_values(Vals, _Oid, #index{table_name = IxTab,
				    type = set}, Mode) ->
    case Mode of
	dirty ->
	    lists:foreach(
	      fun(Key) ->
		      mnesia:dirty_delete(IxTab, Key)
	      end, Vals);
	[VMod, Tid, Ts, LockKind] ->
	    AMod = access_module(IxTab, VMod),
	    lists:foreach(
	      fun(Key) ->
		      AMod:delete(
			Tid, Ts, IxTab, Key, LockKind)
	      end, Vals)
    end;
delete_ix_values(Vals, Oid, #index{table_name = IxTab,
				   type = bag}, Mode) ->
    case Mode of
	dirty ->
	    lists:foreach(
	      fun(Key) ->
		      mnesia:dirty_delete_object(
			IxTab, #ix{key = Key, oid = Oid})
	      end, Vals);
	[VMod, Tid, Ts, LockKind] ->
	    AMod = access_module(IxTab, VMod),
	    lists:foreach(
	      fun(Key) ->
		      AMod:delete_object(
			Tid, Ts, IxTab, #ix{key = Key, oid = Oid}, LockKind)
	      end, Vals)
    end.

check_if_unique(#index{options = Opts, type = Type} = Ix, 
		NewIxValues, Tab, Oid, VMod, Mode) ->
    case (Type == set) orelse proplists:get_value(unique, Opts, false) of
	false ->
	    ok;
	true ->
	    IxTab = Ix#index.table_name,
	    lists:foreach(
	      fun(IxValue) ->
		      Objs =
			  case Mode of
			      dirty ->
				  mnesia:dirty_read({IxTab, IxValue});
			      [VMod, Tid, Ts, LockKind] ->
				  rdbms:read(
				    Tid,Ts,IxTab,IxValue,LockKind,VMod)
			  end,
		      lists:foreach(
			fun(#ix{key = IxKey, oid = Id}) when Id =/= Oid ->
				mnesia:abort({unique_ix_violation,
					      [Tab,Ix#index.pos,IxKey]});
			   (#ix{oid = Id}) when Id == Oid ->
				ok
			end, Objs)
	      end, NewIxValues)
    end.

insert_ix_values(Vals, Oid, #index{table_name = IxTab,
				   type = ordered}, Mode) ->
    case Mode of
	dirty ->
	    lists:foreach(
	      fun(Key) ->
		      mnesia:do_dirty_write(
			async_dirty, IxTab, #ord_ix{key = {Key, Oid}})
	      end, Vals);
	[VMod, Tid, Ts, LockKind] ->
	    AMod = access_module(IxTab, VMod),
	    lists:foreach(
	      fun(Key) ->
		      AMod:write(
			Tid, Ts, IxTab, #ord_ix{key = {Key, Oid}}, LockKind)
	      end, Vals)
	end;
insert_ix_values(Vals, Oid, #index{table_name = IxTab,
				   type = weighted}, Mode) ->
    case Mode of
	dirty ->
	    lists:foreach(
	      fun({Key, Weight}) ->
		      mnesia:do_dirty_write(
			async_dirty, IxTab, #ord_ix{key = {Key, Weight, Oid}})
	      end, Vals);
	[VMod, Tid, Ts, LockKind] ->
	    AMod = access_module(IxTab, VMod),
	    lists:foreach(
	      fun({Key, Weight}) ->
		      AMod:write(
			Tid, Ts, IxTab, #ord_ix{key = {Key, Weight, Oid}},
			LockKind)
	      end, Vals)
	end;
insert_ix_values(Vals, Oid, #index{table_name = IxTab,
				   type = Type}, Mode)
  when Type == bag; Type == set ->
    case Mode of 
	dirty ->
	    lists:foreach(
	      fun(Key) ->
		      mnesia:do_dirty_write(
			async_dirty, IxTab, #ix{key = Key, oid = Oid})
	      end, Vals);
	[VMod, Tid, Ts, LockKind] ->
	    AMod = access_module(IxTab, VMod),
	    lists:foreach(
	      fun(Key) ->
		      AMod:write(
			Tid, Ts, IxTab, #ix{key = Key, oid = Oid}, LockKind)
	      end, Vals)
    end.

base_tab(Tab, VMod) ->
    case access_module(Tab, VMod) of
	mnesia_frag ->
	    table_info(Tab, base_table, VMod);
	_ ->
	    Tab
    end.

    

select_pattern(#index{type = Type}, Key) ->
    KeyPat = if is_tuple(Key) ->
		     {Key};
		true ->
		     Key
	     end,
    case Type of
	ordered ->
	    MatchPat = #ord_ix{key = {'$1','$2'}, _ = '_'},
	    [{MatchPat, [{'==', '$1', KeyPat}], ['$2']}];
	weighted ->
	    MatchPat = #w_ix{key = {'$1','$2','$3'}, _ = '_'},
	    [{MatchPat, [{'==', '$1', KeyPat}], [{{'$3', '$2'}}]}];
	T when T==set; T==bag ->
	    MatchPat = #ix{key = '$1', oid = '$2'},
	    [{MatchPat, [{'==', '$1', KeyPat}], ['$2']}]
    end.


%% TODO Fix so that it works even for single attribute index callbacks
filter_found_set(Set, #index{pos = 1,  %% does this work at all?
			     m_f = {M, F},
			     arg = Arg}, Key) ->
    filter_found_set1(Set, M, F, Arg, Key, []);
filter_found_set(Set, #index{pos = Pos,
			    m_f = {M, F},
			    arg = Arg}, Key) ->
    filter_found_set1(Set, Pos, M, F, Arg, Key, []).

%% Index function operates on whole object
filter_found_set1([[]|T], M, F, Arg, Key, Acc) ->
    filter_found_set1(T, M, F, Arg, Key, Acc);
filter_found_set1([Set|T], M, F, Arg, Key, Acc) ->
    Acc2 =
	lists:foldr(
	  fun(Obj, Acc1) ->
		  Keys = M:F(Obj, Arg),
		  case lists:member(Key, Keys) of
		      true ->
			  [Obj | Acc1];
		      false ->
			  Acc1
		  end
	  end, Acc, Set),
    filter_found_set1(T, M, F, Arg, Key, Acc2);
filter_found_set1([], _, _, _, _, Acc) ->
    Acc.

%% Index function operates on single attribute
filter_found_set1([[]|T], Pos, M, F, Arg, Key, Acc) ->
    filter_found_set1(T, Pos, M, F, Arg, Key, Acc);
filter_found_set1([Set|T], Pos, M, F, Arg, Key, Acc) ->
    Acc2 =
	lists:foldr(
	  fun(Obj, Acc1) ->
		  Keys = M:F(element(Pos, Obj), Arg),
		  case lists:member(Key, Keys) of
		      true ->
			  [Obj | Acc1];
		      false ->
			  Acc1
		  end
	  end, Acc, Set),
    filter_found_set1(T, Pos, M, F, Arg, Key, Acc2);
filter_found_set1([], _, _, _, _, _, Acc) ->
    Acc.

%% TODO Fix so that it works even for single attribute index callbacks
filter_weighted_set(Set, #index{pos = 1,  %% does this work at all?
				m_f = {M, F},
				arg = Arg}, Key) ->
    filter_weighted_set1(Set, M, F, Arg, Key, []);
filter_weighted_set(Set, #index{pos = Pos,
			       m_f = {M, F},
			       arg = Arg}, Key) ->
    filter_weighted_set1(Set, Pos, M, F, Arg, Key, []).

%% Index function operates on whole object
filter_weighted_set1([[]|T], M, F, Arg, Key, Acc) ->
    filter_weighted_set1(T, M, F, Arg, Key, Acc);
filter_weighted_set1([Set|T], M, F, Arg, Key, Acc) ->
    Acc2 =
	lists:foldr(
	  fun({Obj,Weight}, Acc1) ->
		  Keys = M:F(Obj, Arg),
		  case lists:member(Key, Keys) of
		      true ->
			  [{Obj, Weight} | Acc1];
		      false ->
			  Acc1
		  end
	  end, Acc, Set),
    filter_weighted_set1(T, M, F, Arg, Key, Acc2);
filter_weighted_set1([], _, _, _, _, Acc) ->
    Acc.

%% Index function operates on single attribute
filter_weighted_set1([[]|T], Pos, M, F, Arg, Key, Acc) ->
    filter_weighted_set1(T, Pos, M, F, Arg, Key, Acc);
filter_weighted_set1([Set|T], Pos, M, F, Arg, Key, Acc) ->
    Acc2 =
	lists:foldr(
	  fun({Obj, Weight}, Acc1) ->
		  Keys = M:F(element(Pos, Obj), Arg),
		  case lists:member(Key, Keys) of
		      true ->
			  [{Obj, Weight} | Acc1];
		      false ->
			  Acc1
		  end
	  end, Acc, Set),
    filter_weighted_set1(T, Pos, M, F, Arg, Key, Acc2);
filter_weighted_set1([], _, _, _, _, _, Acc) ->
    Acc.




match_object(Tid, Ts, Tab, Pat, #index{pos = Pos, 
				       table_name = IxTab} = Index,
	     LockKind, VMod) when is_integer(Pos) ->
    IxPat = element(Pos, Pat),
    SelectPat = select_pattern(Index, IxPat),
    Keys = mnesia:select(Tid, Ts, IxTab, SelectPat, read),
    TabType = table_info(Tab, type, VMod),
    AMod = access_module(Tab, VMod),
    Tmp = ets:new(tmp, [TabType]),
    lists:foreach(
      fun(Key) ->
	      Objs = AMod:read(Tid, Ts, Tab, Key, LockKind),
	      ets:insert(Tmp, Objs)
      end, Keys),
    Result = ets:match_object(Tmp, Pat),
    ets:delete(Tmp),
    Result.

dirty_match_object(Tab, Pat, #index{pos = Pos,
				    table_name = IxTab} = Index)
  when is_integer(Pos) ->
    %% Assume that we are on the node where the replica is
    IxPat = element(Pos, Pat),
    SelectPat = select_pattern(Index, IxPat),
    Keys = mnesia:dirty_select(IxTab, SelectPat),
    TabType = mnesia:table_info(Tab, type),
    Tmp = ets:new(tmp, [TabType]),
    lists:foreach(
      fun(Key) ->
	      Objs = mnesia:dirty_read(Tab, Key),
	      ets:insert(Tmp, Objs)
      end, Keys),
    Result = ets:match_object(Tmp, Pat),
    ets:delete(Tmp),
    Result.


dirty_read(Tab, IxKey, Index, VMod) ->
    #index{table_name = IxTab} = Ix = index_record(Tab, Index, VMod),
    Oids = mnesia:dirty_rpc(IxTab, ?MODULE, dirty_read2,
			    [Tab, Ix, IxKey]),
    mnesia:dirty_rpc(Tab, ?MODULE, dirty_read3,
		     [Tab, Oids, Ix, IxKey]).


dirty_read2(_Tab, #index{table_name = IxTab} = Ix, IxKey) ->
    Pat = select_pattern(Ix, IxKey),
    mnesia:dirty_select(IxTab, Pat).

dirty_read3(Tab, Oids, #index{type = Type} = Ix, IxKey) ->
    %% TODO:
    %% While this function should work even with fragmented tables,
    %% it is far from optimal for it -- and violates sort order.  FIX!!
    IsOrdered = (Type == ordered) or (Type == weighted),
    VMod = rdbms:default_verification_module(),  % since this is done remotely
    Objs = r_keys(Oids, Tab, []),
    Objs1 = 
	case table_info(Tab, setorbag, VMod) of
	    bag ->
		%% Remove all tuples which don't include Ixkey
		%% FIXME: doesn't work with fun indicies
%%%		Pos = attr_pos(Tab, IxPos, VMod),
%%% 		case Ix#index.m_f of
%%% 		    {?MODULE, pos_index} ->
%%% 			mnesia_lib:key_search_all(
%%% 			  IxKey, Pos, Objs);
%%% 		    _ ->
		IxVal = index_value_fun(Tab, Ix, VMod),
		case Type of
		    weighted ->
			[Obj || Obj <- Objs,
				lists:keymember(1, IxVal(Obj))];
		    _ ->
			[Obj || Obj <- Objs,
				lists:member(IxKey, IxVal(Obj))]
		end;
	    _ -> 
		Objs
	end,
    if IsOrdered ->
	    lists:reverse(Objs1);
       true ->
	    Objs1
    end.

r_keys([H|T],Tab,Ack) -> 
    V = mnesia_lib:db_get(Tab, H),
    r_keys(T, Tab, V ++ Ack);
r_keys([], _, Ack) ->
    Ack.


dirty_index_first(Tab, #index{table_name = IxTab} = Index) ->
    mnesia:dirty_rpc(IxTab, ?MODULE, dirty_index_first, [first, Tab, Index]).

dirty_index_last(Tab, #index{table_name = IxTab} = Index) ->
    mnesia:dirty_rpc(IxTab, ?MODULE, dirty_index_first, [last, Tab, Index]).

dirty_index_first(FirstOrLast, Tab,
		  #index{pos = Pos,
			 table_name = IxTab,
			 type = Type}) ->
    Storage = mnesia_lib:storage_type_at_node(node(), IxTab),
    Res = case Storage of
	      ram_copies ->
		  ets:FirstOrLast(IxTab);
	      disc_only_copies ->
		  dets:FirstOrLast(IxTab)
	  end,
    case Res of
	'$end_of_table' = R ->
	    R;
	IxKey ->
	    case Type of
		weighted ->
		    {Ix, _Weight, _FirstObjKey} = IxKey,
		    mnesia:dirty_index_read(Tab, Ix, Pos);
		ordered ->
		    {Ix, _FirstObjKey} = IxKey,
		    mnesia:dirty_index_read(Tab, Ix, Pos);
		T when T==set; T==bag ->
		    {IxKey, mnesia:dirty_index_read(Tab, IxKey, Pos)}
	    end
    end.

dirty_index_next(Tab, #index{table_name = IxTab} = Index, IxKey) ->
    mnesia:dirty_rpc(IxTab, ?MODULE, dirty_index_relop,
		     [next, Tab, Index, IxKey]).

dirty_index_prev(Tab, #index{table_name = IxTab} = Index, IxKey) ->
    mnesia:dirty_rpc(IxTab, ?MODULE, dirty_index_relop,
		     [prev, Tab, Index, IxKey]).

dirty_index_relop(
  Direction, Tab, #index{pos = Pos,
			 table_name = Ixt,
			 type = Type} = Index, IxKey) ->
    Storage = mnesia_lib:storage_type_at_node(node(), Ixt),
    IsOrdered = (Type == ordered) or (Type == weighted),
    Next = 
	case {Storage, Direction, IsOrdered} of
	    {ram_copies, prev, true} ->
		{Ix,_} = IxKey,
		Pat = select_pattern(Index, Ix),
		case ets:select(Ixt, Pat, 1) of
		    '$end_of_table' = R1 ->
			R1;
		    {[FirstObjKey], _Cont} ->
			ets:prev(Ixt, {Ix,FirstObjKey})
		end;
	    {ram_copies,_,_} ->
		ets:Direction(Ixt, IxKey);
	    {disc_only_copies,_,_} ->
		dets:Direction(Ixt, IxKey)
	end,
    case Next of
	'$end_of_table' = R2 ->
	    R2;
	NewIxKey ->
	    case IsOrdered of
		true ->
		    {NewIx, _FirstObjKey} = NewIxKey,
		    Objs = mnesia:dirty_index_read(Tab, NewIx, Pos),
		    LastObj = lists:last(Objs),
		    LastObjKey = element(2, LastObj),
		    {{NewIx, LastObjKey}, Objs};
		false ->
		    {NewIxKey, mnesia:dirty_index_read(Tab, NewIxKey, Pos)}
	    end
    end.



valid_index(Tab, Index) ->
    valid_index(Tab, Index, rdbms:fetch_verification_module()).

valid_index(Tab, Index, VMod) ->
    case catch index_record(Tab, Index, VMod) of
	#index{} ->
	    true;
	_ ->
	    false
    end.


%%% ============ index_read for fragmented tables

frag_read(ActivityId, Opaque, Tab, Key, LockKind) ->
    Frag = key_to_frag_name(Tab, Key),
    mnesia:read(ActivityId, Opaque, Frag, Key, LockKind).


frag_index_read(Tid, Ts, Tab, Key, Attr, LockKind, VMod) 
  when atom(Tab), Tab /= schema ->
    Ix = index_record(Tab, Attr, VMod),
    IxTab = Ix#index.table_name,
    verify_key(Tab, Attr, Key, LockKind),  % aborts if something fishy
    {Oids, Nodes} = 
        case Ix#index.type of
	    ordered ->
                Pat = select_pattern(Ix, Key),
                Found = mnesia_frag:select(Tid, Ts, IxTab, Pat, LockKind),
                lists:foldl(
                  fun(K, {Os, Ns}) ->
                          Frag = key_to_frag_name(Tab, K),
                          Node = mnesia_lib:val({Frag, where_to_read}),
                          {[{Node, Frag, K}|Os], sets:add_element(Node, Ns)}
                  end, {[], sets:new()}, Found);
	    weighted ->
                Pat = select_pattern(Ix, Key),
                Found = mnesia_frag:select(Tid, Ts, IxTab, Pat, LockKind),
                lists:foldl(
                  fun({K,_}, {Os, Ns}) ->
                          Frag = key_to_frag_name(Tab, K),
                          Node = mnesia_lib:val({Frag, where_to_read}),
                          {[{Node, Frag, K}|Os], sets:add_element(Node, Ns)}
                  end, {[], sets:new()}, Found);
	    T when T==set; T==bag ->
                IxObjs = frag_read(Tid, Ts, IxTab, Key, LockKind),
                lists:foldl(
                  fun(#ix{oid = K}, {Os, Ns}) ->
                          Frag = key_to_frag_name(Tab, K),
                          Node = mnesia_lib:val({Frag, where_to_read}),
                          {[{Node, Frag, K}|Os], sets:add_element(Node, Ns)}
                  end, {[], sets:new()}, IxObjs)
        end,
    %% This can be improved: right now we send all keys to all nodes
    %% and let the receiving end figure out which objects to read.
    FoundSet = 
        case rpc:multicall(
               sets:to_list(Nodes), 
               lists, foldl,
               [fun({N, Frag, K}, Acc) when N == node() ->
                        mnesia:read(Tid, Ts, Frag, K, read) ++ Acc;
                   (_, Acc) ->
                        Acc
                end, [], Oids]) of
            {Replies, []} ->
                Replies;
            {_Replies, BadNodes} ->
                mnesia:abort({badarg, BadNodes})
        end,
    case val({Tab, setorbag}) of
        bag ->
            filter_found_set(FoundSet, Ix, Key);
        _ ->
            lists:append(FoundSet)
    end;
frag_index_read(_Tid, _Ts, Tab, _Key, _Attr, _LockKind, _VMod) ->
    mnesia:abort({bad_type, Tab}).


verify_key(Tab, Attr, Key, read) ->
    case mnesia:has_var(Key) of
        false ->
            ok;
        true ->
            mnesia:abort({bad_type, Tab, Attr, Key})
    end;
verify_key(Tab, _Attr, _Key, LockKind) ->
    mnesia:abort({bad_type, Tab, LockKind}).


%%% ============ end index_read for fragmented tables


do_add_indexes(_Name, []) ->
    ok;
do_add_indexes(TabName, [_|_] = Indexes) ->
    %% TODO: must make sure that index is brought up-to-date
    TabInfo = mnesia_schema:do_read_table_info(TabName),
    io:format("TabInfo = ~p~n", [TabInfo]),
    BaseCs = mnesia_schema:list2cs(maybe_add_name(TabName, TabInfo)),
    io:format("BaseCs = ~p~n", [BaseCs]),
    Attrs = BaseCs#cstruct.attributes,
    RecName = BaseCs#cstruct.record_name,
    IndexRecs = 
	lists:map(
	  fun({{P, Tag}, M, F, Info, Options} = Ix) ->
		  P1 = if
			   is_integer(P) ->
			       P;
			   P == RecName ->
			       1;
			   is_atom(P) ->
			       io:format("HERE!~n"
					 "recname = ~p~n"
					 "Attrs = ~p~n",
					 [RecName, Attrs]),
			       case pos(P, [RecName|Attrs]) of
				   Pos when Pos == 0; Pos == 2 ->
				       %% cannot put index on primary key(??)
				       mnesia:abort({bad_type, {index, Ix}});
				   Pos ->
				       Pos
			       end;
			   true ->
			       mnesia:abort({bad_type, {index, Ix}})
		       end,
		  #index{pos = {P1, Tag},
			 m_f = {M, F},
			 arg = Info,
			 type = index_type(Options),
			 tab_opts = index_tab_options(Options),
			 table_name = index_tab_name(TabName, {P1, Tag}),
			 options = index_other_options(Options)}
	  end, Indexes),
    rdbms_props:do_set_property(TabName, indexes, IndexRecs),
    IxTabInfo =
	[{Ix#index.table_name,
	  mnesia_schema:cs2list(index_table(TabName, Ix, BaseCs))} ||
	    Ix <- IndexRecs],
    case parent_table_loaded(TabName) of
	false ->
	    io:format("parent_table_loaded(~p)-> false.~n", [TabName]),
	    do_create_index_tabs(IndexRecs, TabName, BaseCs);
%%%    fill_indexes(IndexRecs, TabName).
	true ->  % table existed before this transaction
	    io:format("parent_table_loaded(~p)-> true.~n", [TabName]),
	    Prep = mnesia_schema:prepare_restore(
		     {IndexRecs, IxTabInfo, TabName},
		     [{default_op, recreate_tables}],
		     rdbms_index_load),
	    mnesia_schema:do_restore(Prep)
    end.

parent_table_loaded(Tab) ->
    case mnesia_lib:val({Tab,where_to_read}) of
	nowhere -> false;
	_ ->
	    true
    end.

maybe_add_name(Name, [{name,Name}|_] = Info) ->
    Info;
maybe_add_name(Name, [{K,_}|_] = Info) when K =/= Name ->
    [{name, Name}|Info].

%%% fill_indexes(Ixs, Tab) ->
%%%     {_Mod, Tid, Ts} = get(mnesia_activity_state),
%%%     mnesia:write_lock_table(Tab),
%%%     VMod = rdbms:fetch_verification_module(),
%%%     case table_info(Tab, size, VMod) of
%%% 	0 ->
%%% 	    ok;
%%% 	_N ->
%%% 	    io:format("Filling indexes from ~p (~p)~n", [Tab, Ixs]),
%%% 	    Fun = fun(Obj, _Acc) ->
%%% 			  update_indexes(
%%% 			    Ixs, Tid, Ts, Tab, fill, Obj, write, VMod),
%%% 			  ok
%%% 		  end,
%%% 	    rdbms:foldl(Tid, Ts, Fun, ok, Tab, write)
%%%     end.
    

pos(X, List) ->
    pos(X, List, 1).
pos(X, [X|_], P) ->
    P;
pos(X, [_|T], P) ->
    pos(X, T, P+1);
pos(_X, [], _) ->
    0.

index_tab_name(Tab, Pos) ->
    Prefix = "-RDBMS-NDX-",
    case Pos of
	N when is_integer(N) ->
	    list_to_atom(lists:append([Prefix,
				       atom_to_list(Tab), "-",
				       integer_to_list(N)]));
	{N, Tag} when is_integer(N), is_atom(Tag) ->
	    list_to_atom(lists:append([Prefix,
				       atom_to_list(Tab), "-",
				       integer_to_list(N), "-",
				       atom_to_list(Tag)]))
    end.

index_type(Opts) when is_list(Opts) ->
    case lists:keysearch(type, 1, Opts) of
	{value, {_, T}} ->
	    case lists:member(T, [ordered, weighted, set, bag]) of
		true ->
		    T;
		false ->
		    mnesia:abort({bad_type, T})
	    end;
	false ->
	    bag
    end.
%%% index_is_ordered(Opts) when is_list(Opts) ->
%%%     case lists:keysearch(type, 1, Opts) of
%%% 	{value, {_, ordered_set}} ->
%%% 	    true;
%%% 	_ ->
%%% 	    false
%%%     end.

index_tab_options(Opts) when is_list(Opts) ->
    case lists:keysearch(tab_options, 1, Opts) of
	{value, {_, TabOpts}} ->
	    TabOpts;
	false ->
	    []
    end.

index_other_options(Opts) ->
    lists:filter(
      fun({tab_options,_}) ->
	      false;
	 ({type, _}) ->
	      false;
	 ({unique,B}) when is_boolean(B) ->
	      true;
	 (Other) ->
	      mnesia:abort({invalid_index_option, Other})
      end, Opts).


do_create_index_tabs(IndexRecs, TabName, TabCs) ->
    Tabs = [index_table(TabName, Ix, TabCs) || Ix <- IndexRecs],
    lists:foreach(
      fun(IxTabCs) ->
 	      mnesia_schema:do_create_table(IxTabCs)
      end, Tabs).


index_table(Tab, #index{pos = Pos, 
			type = Type,
			tab_opts = TabOpts}, TabCs) ->
    %% inherit table's
    %% - replication scheme
    %% - local_content flag
    %% - load_order
    %% - frag_properties (?)
    {TType, RecName, Attrs} =
	case Type of
	    set      -> {set,         ix,     record_info(fields, ix)};
	    bag      -> {bag,         ix,     record_info(fields, ix)};
	    ordered  -> {ordered_set, ord_ix, record_info(fields, ord_ix)};
	    weighted -> {ordered_set, w_ix,   record_info(fields, w_ix)}
	end,
    IxTabName = index_tab_name(Tab, Pos),
    Cs0 = 
	TabCs#cstruct{
	  name = IxTabName,
	  type = TType,
	  load_order = TabCs#cstruct.load_order + 1,  % load index bef main tab
	  index = [],
	  frag_properties = case TabCs#cstruct.frag_properties of
				[{base_table, Tab}|Rest] ->
				    %% mnesia_frag adds base_table later
				    lists:keydelete(hash_state, 1, Rest);
				[] ->
				    []
			    end,
	  snmp = [],
	  record_name = RecName,
	  attributes = Attrs,
	  %% local_content inherits the value of the parent tab
	  %% (We should use the acl functionality here instead... TODO)
	  user_properties = [{{tab, access_mode}, index},
			     {{tab, index_properties}, [{parent, Tab}]}]},
    check_ix_tab_opts(Tab, TabOpts, Cs0).


check_ix_tab_opts(Tab, Opts, Cs0) ->
    lists:foldl(
      fun({ram_copies, Ns}, Cs) ->
	      Cs#cstruct{ram_copies = Ns};
	 ({disc_copies, Ns}, Cs) ->
	      Cs#cstruct{disc_copies = Ns};
	 ({disc_only_copies, Ns}, Cs) ->
	      Cs#cstruct{disc_only_copies = Ns};
	 ({local_content, Bool}, Cs) ->
	      Cs#cstruct{local_content = Bool};
	 ({load_order, Order}, Cs) ->
	      Cs#cstruct{load_order = Order};
	 ({user_properties, Props}, Cs) ->
	      Cs#cstruct{user_properties = Props};
	 ({frag_properties, Props}, Cs) ->
	      Cs#cstruct{frag_properties = Props};
	 (Other, _Cs) ->
	      mnesia:abort({bad_type, {index_option, Tab, Other}})
      end, Cs0, Opts).


%%% End do_add_indexes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% do_delete_indexes/1

do_delete_indexes([]) ->
    ok;
do_delete_indexes(Indexes) ->
    lists:foreach(
      fun(#index{table_name = IxTab}) ->
	      %% TODO - How about fragmented indexes?
	      mnesia_schema:do_delete_table(IxTab)
      end, Indexes).

attr_pos(Tab, Index, VMod) ->
    case Index of
	I when is_integer(I) ->
	    I;
	{I, _Tag} when is_integer(I) ->
	    I;
	{Attr, _Tag} when is_atom(Attr) ->
	    attr_tab_to_pos(Tab, Attr, VMod);
	#index{pos = IxPos} ->
	    %% BW compat (???)
	    attr_pos(Tab, IxPos, VMod)
    end.
    
%% Convert attribute name to integer if neccessary
attr_tab_to_pos(_Tab, Pos, _VMod) when integer(Pos) ->
    Pos;
attr_tab_to_pos(Tab, Attr, VMod) ->
    attr_to_pos(Attr, table_info(Tab, attributes, VMod)).
    
%% Convert attribute name to integer if neccessary
attr_to_pos(Pos, _Attrs) when integer(Pos) ->
    Pos;
attr_to_pos(Attr, Attrs) when atom(Attr) ->
    attr_to_pos(Attr, Attrs, 2);
attr_to_pos(Attr, _) ->
    mnesia:abort({bad_type, Attr}).

attr_to_pos(Attr, [Attr | _Attrs], Pos) ->
    Pos;
attr_to_pos(Attr, [_ | Attrs], Pos) ->
    attr_to_pos(Attr, Attrs, Pos + 1);
attr_to_pos(Attr, _, _) ->
    mnesia:abort({bad_type, Attr}).



index_record(Tab0, Index, VMod) ->
    Tab = base_tab(Tab0, VMod),
    IxId = case Index of
	       I when is_integer(I) ->
		   I;
	       Attr when is_atom(Attr) ->
		   attr_tab_to_pos(Tab, Attr, VMod);
	       {I, _Tag} when is_integer(I) ->
		   Index;
	       {Attr, Tag} when is_atom(Attr) ->
		   P = attr_tab_to_pos(Tab, Attr, VMod),
		   {P, Tag}
	   end,
    Ixes = table_indexes(Tab, VMod),
    case lists:keysearch(IxId, #index.pos, Ixes) of
	{value, Ix} ->
	    Ix;
	false ->
	    mnesia:abort({bad_type,Tab,Index})
    end.


%%% %% Default callback for old-style mnesia indexes.
%%% %%
%%% pos_index(Val, _Arg) ->
%%%     [Val].



%% Called when table is actually loaded. Figures out which indexes to 
%% rebuild. Basically, local_content ram_copy indexes are always rebuilt,
%% and normal ram_copies are rebuilt the first time only (if there is not
%% already an active copy (replica) somewhere).
index_init_fun(Tab, LoadReason) ->
    VMod = rdbms:fetch_verification_module(),
    index_init_fun(Tab, mnesia_lib:val({Tab, cstruct}), LoadReason, VMod).

index_init_fun(Tab, Cs, LoadReason) ->
    index_init_fun(Tab, Cs, LoadReason, rdbms:fetch_verification_module()).

index_init_fun(Tab, _Cs, LoadReason, VMod) ->
    Indexes = table_indexes(Tab, VMod),
    io:format("~p - Indexes(~p) = ~p, LoadReason=~p~n",
	      [self(), Tab,Indexes,LoadReason]),
    FirstLoad = case LoadReason of
		    initial ->	true;
		    local_only -> true;
		    local_master -> true;
		    _ ->
			false
		end,
    try init_indexes(Tab, FirstLoad, Indexes)
    catch
	throw:requeue ->
	    requeue
    end.

init_indexes(_Tab, FirstLoad, Indexes) ->
    InitIndexes =
	lists:foldl(
	  fun(#index{table_name = IxTab} = Ix, Acc) ->
		  Cs = mnesia_lib:val({IxTab, cstruct}),
		  io:format("Cs = ~p~n", [Cs]),
		  StorageType = mnesia_lib:cs_to_storage_type(node(), Cs),
		  case StorageType of
		      disc_only_copies ->
			  %% we should really check somewhere that 
			  %% the index is consistent with the table.
			  %% For now, we just assume it is.
			  Acc;
		      disc_copies ->
			  %% we should really check somewhere that 
			  %% the index is consistent with the table.
			  %% For now, we just assume it is.
			  Acc;
		      ram_copies ->
			  io:format("Ix = ~p~n"
				    "Dict = ~p~n"
				    "ProcI = ~p~n",
				    [Ix, get(), process_info(self())]),
			  case Cs#cstruct.local_content of
			      true ->
				  [Ix|Acc];
			      false ->
				  case FirstLoad of
				      true ->
					  io:format("Ix = ~p~n"
						    "Dict = ~p~n"
						    "ProcI = ~p~n",
						    [Ix, get(),
						     process_info(self())]),
					  [Ix|Acc];
				      false ->
					  %% Not local_content
					  %% + already loaded ->
					  %% index should already be
					  %% available, and
					  %% we don't have to do anything.
					  Acc
				  end
			  end
		  end
	  end, [], Indexes),
    io:format("~p - InitIndexes = ~p~n", [self(), InitIndexes]),
    _F = 
	fun(start) ->
		io:format("fun(start)~n", []),
		{ok,{_,Tid,Ts}} =
		    mnesia_tm:begin_activity(async,rdbms),
		Store = Ts#tidstore.store,
		lists:foreach(
		  fun(#index{table_name = IxTab}) ->
			  mnesia_locker:wlock_table(Tid,Store,IxTab)
		  end, InitIndexes),
		io:format("fun(start) worked~n", []);
	   (done) ->
		io:format("fun(done)~n", []),
		{_, _, _} = TidTs = get(mnesia_activity_state),
		mnesia_tm:commit_activity(ok, async, TidTs),
		io:format("fun(done) worked~n", []);
	   (Objs) ->
		lists:foreach(
		  fun(Ix) ->
			  lists:foreach(
			    fun({Op, Obj}) ->
				    update_index(Op, Obj, Ix)
			    end, Objs)
		  end, InitIndexes)
	end.


index_value_fun(Tab, #index{pos = Pos, m_f = {M,F}, arg = Arg}, VMod) ->
    case attr_pos(Tab, Pos, VMod) of
	1 ->
	    fun(Obj) ->
		    M:F(Obj, Arg)
	    end;
	P when P > 1 ->
	    fun(Obj) ->
		    M:F(element(P, Obj), Arg)
	    end
    end.


update_index(Op, Obj, #index{pos = Pos,
			     m_f = {Mod, Fun},
			     arg = Arg,
			     type = Type,
			     table_name = Tab}) ->
    Oid = element(2, Obj),
    Keys = 
        case Pos of
            {1,_} ->
                Mod:Fun(Obj, Arg);
            P when is_integer(P), P > 0 ->
                Value = element(P, Obj),
                Mod:Fun(Value, Arg);
            {P,_} when is_integer(P), P > 0 ->
                Value = element(P, Obj),
                Mod:Fun(Value, Arg)
        end,
    F = 
	case {Type,Op} of
	    {ordered,write} ->
		fun(K) -> mnesia_lib:db_put(Tab, #ord_ix{key = {K, Oid}}) end;
	    {ordered,Del} when Del==delete; Del==delete_object ->
		fun(K) -> mnesia_lib:db_erase(Tab, {K, Oid}) end;
	    {weighted,write} ->
		fun({K,W}) ->
			mnesia_lib:db_put(Tab, #w_ix{key = {K, W, Oid}})
		end;
	    {weighted,Del} when Del==delete; Del==delete_object ->
		fun({K,W}) -> mnesia_lib:db_erase(Tab, {K,W,Oid}) end;
	    {T, write} when T==set; T==bag ->
		fun(K) ->
			mnesia_lib:db_put(Tab, #ix{key = K, oid = Oid})
		end;
	    {set, Del} when Del==delete; Del==delete_object ->
		fun(K) -> mnesia_lib:db_erase(Tab, K) end;
	    {bag, Del} when Del==delete; Del==delete_object ->
		fun(K) ->
			mnesia_lib:db_match_erase(
			  Tab, #ix{key = K, oid=Oid, _='_'})
		end
	end,
    lists:foreach(F, Keys).
%%%     lists:foreach(F, Keys).
%%%             lists:foreach(
%%%               fun(K) ->
%%%                       mnesia_lib:db_put(Tab, #ord_ix{key = {K, Oid}})
%%%               end, Keys);
%%%         weighted ->
%%%             lists:foreach(
%%%               fun({K,W}) ->
%%%                       mnesia_lib:db_pu(Tab, #w_ix{key = {K, W, Oid}})
%%%               end, Keys);
%%% 	bag ->
%%%             lists:foreach(
%%%               fun(K) ->
%%%                       mnesia_lib:db_put(Tab, #ix{key = K, oid = Oid})
%%%               end, Keys)
%%%     end.


val(Var) ->
    case ?catch_val(Var) of
        {'EXIT', _ReASoN_} -> mnesia_lib:other_val(Var, _ReASoN_); 
        _VaLuE_ -> _VaLuE_ 
    end.

