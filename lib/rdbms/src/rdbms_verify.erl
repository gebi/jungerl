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
%%% File    : rdbms_verify.erl
%%% Author  : Ulf Wiger <etxuwig@ws12858>
%%% Description : "Slow-path" verification code for rdbms
%%%
%%% Created : 26 Dec 2005 by Ulf Wiger <etxuwig@ws12858>
%%%-------------------------------------------------------------------
-module(rdbms_verify).

-export([validate_rec/3, indexes/2, acl/2, 
	 verify_access/4,
	 references/2,
	 rec_type/2,
	 module/2]).
-export([check_type/2,    % (Type, Value)
	 check_type/3,    % (Type, Value, ErrInfo)
	 filter_obj/3,
	 check_access/4,
	 access_filter/3,
	 on_read/3]).

-export([table_property/4, attr_property/5, table_info/3]).
-include("rdbms.hrl").

indexes(Tab, #verify{tab_property = Prop}) ->
    Prop(Tab, indexes, []).

references(Tab, VRec) ->
    table_property(Tab, references, VRec, []).

rec_type(Tab, VRec) ->
    table_property(Tab, rec_type, VRec, no_type).

acl(Tab, VRec) ->
    table_property(Tab, acl, VRec, []).

module(Tab, #verify{table_info = TI}) ->
    case TI(Tab, frag_properties) of
	[] ->
	    mnesia;
	[_|_] ->
	    mnesia_frag
    end.


verify_access(Tab, Rec, Mode, VRec) ->
    case acl(Tab, VRec) of
	[] ->
	    true;
	[_|_] = Acl->
	    check_access(Acl, Mode, Rec, Tab)
    end.

check_access([{M, What}|_], Mode, Rec, Tab) when M==Mode; M=='_' ->
    case access_return(What, Tab, Mode, Rec) of
	true -> true;
	false ->
	    mnesia:abort({access_violation,[Tab,Mode]})
    end;
check_access([_|T], Mode, Rec, Tab) ->
    check_access(T, Mode, Rec, Tab);
check_access([], _, _, _) ->
    true.

access_filter([], Objs, _Tab) ->
    Objs;
access_filter(Acl, Objs, Tab) ->
    What = read_access_what(Acl),
    lists:filter(fun(Obj) ->
			 access_return(What, Tab, read, Obj)
		 end, Objs).

read_access_what([{read,What}|_]) ->
    What;
read_access_what([{'_',What}|_]) ->
    What;
read_access_what([_|T]) ->
    read_access_what(T);
read_access_what([]) ->
    true.

access_return(What, Tab, Mode, Rec) ->
    case What of
	Bool when is_boolean(Bool) ->
	    Bool;
	{Mod,Fun} ->
	    true == Mod:Fun(Tab, Mode, Rec)
    end.


validate_rec(Tab, Rec, VRec) ->
    case rec_type(Tab, VRec) of
	no_type -> true;
	RecT ->
	    case check_type(RecT, Rec) of
		false ->
		    mnesia:abort({type_error, [Tab, RecT, RecT]});
		true ->
		    callback_validate(Rec, Tab, VRec)
	    end
    end.

on_read(Tab, Objs, VRec) ->
    Acl = acl(Tab, VRec),
    What = read_access_what(Acl),
    RFilter = table_property(Tab, read_filter, VRec, no_filter),
    case {What, RFilter} of
	{false, _} ->
	    [];
	{true, no_filter} ->
	    Objs;
	{true, RFilter} ->
	    rdbms_ms:run_ms(Objs, RFilter);
%%% 	    ets:match_spec_run(Objs, ets:match_spec_compile(RFilter));
	{{M,F}, no_filter} ->
	    lists:filter(
	      fun(Obj) ->
		      true == M:F(Tab, read, Obj)
	      end, Objs);
	{{M,F}, RFilter} ->
	    lists:filter(
	      fun(Obj) ->
		      true == M:F(Tab, read, Obj)
	      end,
	      rdbms_ms:run_ms(Objs, RFilter))
%%% 	      ets:match_spec_run(Objs,
%%% 				      ets:match_spec_compile(RFilter)))
    end.
		

	    

table_property(Tab, Prop, #verify{tab_property = TP}, Default) ->
    TP(Tab, Prop, Default).

attr_property(Tab, Attr, Prop, #verify{attr_property = AP}, Default) ->
    AP(Tab, Attr, Prop, Default).

table_info(Tab, InfoItem, #verify{table_info = TI}) ->
    TI(Tab, InfoItem).


filter_obj(Obj, Pattern, Info) ->
    case rdbms_ms:run_ms([Obj], Pattern) of
%%%     case ets:match_spec_run([Obj], ets:match_spec_compile(Pattern)) of
	[] ->
	    mnesia:abort({filter_violation, Info});
	[NewObj] ->
	    NewObj
    end.

	


%%% attribute_type(Tab, Attr, #verify{attr_property = AP}) ->
%%%     AP({Tab,Attr}, type, undefined).

check_type(Type, Value, Info) ->
    case check_type(Type, Value) of
	false ->
	    mnesia:abort({type_violation, Info});
	true ->
	    true
    end.

check_type(false, _) -> false;
check_type(true, _) -> true;
check_type(no_type, _) -> true;
check_type(any, _) -> true;
check_type(undefined, undefined) -> true;
check_type({enum, Vs}, X) -> lists:member(X, Vs);
check_type({tuple, Arity, Ts}, X) when is_tuple(X), size(X) == Arity ->
    Elems = tuple_to_list(X),
    lists:zipwith(fun check_type/2, Ts, Elems);
check_type({tuple, Arity}, X) when is_tuple(X), size(X) == Arity -> true;
check_type(tuple, X) when is_tuple(X) -> true;
check_type(integer, X) when is_integer(X) -> true;
check_type(float, X) when is_float(X) -> true;
check_type(number, X) when is_integer(X); is_float(X) -> true;
check_type(atom, X) when is_atom(X) -> true;
check_type(pid, X) when is_pid(X) -> true;
check_type(port, X) when is_port(X) -> true;
check_type(binary, X) when is_binary(X) -> true;
check_type(list, X) when is_list(X) -> true;
check_type(string, X) ->
    try list_to_binary(X) of 
	_ -> true
    catch
	error:_ -> false
    end;
check_type(nil, [])  -> true;
check_type({list,false}, _) -> false; % optimization
check_type({list, T}, [_|_]) when T==any; T==undefined -> true;
check_type({list, T}, X) when is_list(X) ->
    while(true, fun(Elem) -> check_type(T, Elem) end, X);
check_type(function, X) when is_function(X) -> true;
check_type({function,Arity}, X) when is_function(X, Arity) -> true;
%%% check_type({'and',Ts}, X) ->
%%%     lists:foldl(fun(T,Acc) ->
%%% 		     Acc and check_type(T,X)
%%% 		end, true, Ts);
check_type({'and',Ts}, X) ->
    %% 'andalso' semantics
    while(true, fun(T) -> check_type(T, X) end, Ts);
%%% check_type({'or',Ts}, X) ->
%%%     lists:foldl(fun(T,Acc) ->
%%% 			Acc or check_type(T,X)
%%% 		end, false, Ts);
check_type({'or', Ts}, X) ->
    %% 'orelse' semantics
    while(false, fun(T) -> check_type(T, X) end, Ts);
check_type({'not', T}, X) ->
    not(check_type(T, X));
check_type({O,V}, X) when O=='==';O=='=/=';O=='<';O=='>';O=='>=';O=='=<' ->
    erlang:O(X,V);
check_type(oid, {Nd, {Ms,S,Us}})
  when is_atom(Nd), is_integer(Ms), is_integer(S), is_integer(Us),
       Ms >= 0, S >= 0, Us >= 0 ->
    %% c.f. {node(), erlang:now()}
    true;
check_type(_, _) ->
    false.
    
while(Bool, F, L) when is_list(L) ->
    Not = not(Bool),
    try lists:foreach(
	  fun(Elem) ->
		  case F(Elem) of
		      Not ->  throw(Not);
		      Bool -> Bool
		  end
	  end, L) of
	_ -> Bool
    catch
	throw:Not ->
	    Not
    end.
    


%% callback_validate(Record, VRec)
%% This allows a user to register his own verification trigger for a certain
%% record. This trigger function should call mnesia:abort() if verification
%% fails.
%%
callback_validate(Rec, Tab, #verify{tab_property = TP}) ->
    case TP(Tab, verify) of
	undefined ->
	    true;
	F ->
	    F(Rec)
    end.

