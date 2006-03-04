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
%%% File    : rdbms_index_load.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : mnesia_recover callback for rebuilding indexes
%%%
%%% Created :  5 Jan 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_index_load).

-include("rdbms.hrl").

-export([open_read/1,
	 read/1,
	 close_read/1]).

-import(rdbms_index, [attr_pos/3,
		      table_info/3]).

open_read({[], [], _}) ->
    {ok, []};
open_read({Ixs, IxTabs, Tab}) ->
    {_,Tid,Ts} = get(mnesia_activity_state),
    VMod = rdbms:fetch_verification_module(),
    {ok, {return_schema, IxTabs, Tab, Tid, Ts, Ixs, VMod}}.

read([]) ->
    {ok, [], []};
read({return_schema, IxTabs, Tab, Tid, Ts, Ixs, VMod}) ->
    Next = {return_data, Tab, Tid, Ts, Ixs, VMod},
    {ok, Next, [mnesia_log:backup_log_header()|
		[{schema,IxTab,List} || {IxTab,List} <- IxTabs]]};
read({return_data, Tab, Tid, Ts, Ixs, VMod}) ->
    mnesia:write_lock_table(Tab),
    case table_info(Tab, size, VMod) of
	0 ->
	    {ok, [], []};
	_N ->
	    io:format("Populating indexes from ~p (~p)~n", [Tab, Ixs]),
	    Fun = fun(Obj, Acc) ->
			  Oid = element(2, Obj),
			  index_values(
			    Ixs, Tab, Oid, Obj, Acc, VMod)
		  end,
	    Items = rdbms:foldr(Tid, Ts, Fun, [], Tab, read),
	    {ok, [], Items}
    end.


close_read(Opaque) ->
    {ok, Opaque}.

index_values(Ixes, Tab, Oid, Obj, Acc0, VMod) ->
    lists:foldl(
      fun(#index{m_f = {M, F}, arg = Arg,
		 table_name = IxTab,
		 type = Type} = Ix, Acc1) ->
	      Pos = attr_pos(Tab, Ix, VMod),
	      Tag = fun(X) -> setelement(1,X,IxTab) end,
	      IxValue = case Type of
			    ordered ->
				fun(X) ->
					Tag(#ord_ix{key={X,Oid}})
				end;
			    weighted ->
				fun({X,W}) ->
					Tag(#ord_ix{key={X,W,Oid}})
				end;
			    bag ->
				fun(X) ->
					Tag(#ix{key=X, oid=Oid})
				end
			end,
	      lists:foldl(
		fun(IxVal, Acc2) ->
			[IxValue(IxVal)|Acc2]
		end, Acc1, get_index_vals(Obj, Pos, M, F, Arg))
      end, Acc0, Ixes).

get_index_vals(Obj, 1, M, F, Arg) ->
    M:F(Obj, Arg);
get_index_vals(Obj, Pos, M, F, Arg) ->
    M:F(element(Pos, Obj), Arg).
