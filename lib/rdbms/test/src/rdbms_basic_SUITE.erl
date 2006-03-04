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
%%% File    : rdbms_basic_SUITE.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : Test suite for rdbms
%%%
%%% Created : 20 Feb 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_basic_SUITE).

-define(default_timeout, ?t:minutes(1)).
-define(application, rdbms).

-define(MNESIA_RECOMPILED, "/.../mnesia-4.2.3ext/ebin").
-define(MNESIA_PATCH_PATH, "$JUNGERL/lib/rdbms/mnesia-patches/ebin").
-define(RDBMS_PATH, "$JUNGERL/lib/rdbms/ebin").

-export([
	 all/1,
	 init_per_testcase/2,
	 fin_per_testcase/2,
	 %%
	 %% basic create/delete tables tests
	 %%
	 install_schema/1,
	 delete_schema/1,
	 basic_create_table/1,
	 rdbms_basic/1,
	 rdbms_create_table/1,
	 rdbms_verify_write_basic/1,
	 rdbms_types_undef/1,
	 rdbms_types_list/1,
	 rdbms_types_enum/1,
	 rdbms_write_filter/1,
	 rdbms_acl/1,
	 rdbms_references/1,
	 rdbms_indexes/1,
	 rdbms_refs_cascade/1,
	 rdbms_refs_lookup/1,
	 rdbms_ix/1,
	 rdbms_ix_load/1,
	 rdbms_add_ix/1,
	 rdbms_unique_ix/1
	]).
-export([word_attr_ix/2,
	 ix_value/2]).
-export([acl2_read/3]).

%%% snipped from test_server.hrl
-ifdef(line_trace).
-line_trace(true).
-define(line,
        put(test_server_loc,{?MODULE,?LINE}),
        io:format(lists:concat([?MODULE,",",integer_to_list(?LINE),": ~p"]),
                  [erlang:now()]),).
-else.
-define(line,put(test_server_loc,{?MODULE,?LINE}),).
-endif.
-define(t,test_server).
-define(config,test_server:lookup_config).
%%% end snip



all(doc) ->
    "rdbms test suite";
all(suite) ->
    [
     install_schema,
     basic_create_table,
%%%     rdbms_create_table,
     rdbms_basic,
     rdbms_indexes,
     rdbms_references,
     delete_schema
    ].

rdbms_references(doc) ->
    [];
rdbms_references(suite) ->
    [rdbms_refs_cascade, rdbms_refs_lookup].

rdbms_indexes(doc) ->
    [];
rdbms_indexes(suite) ->
    [rdbms_ix, rdbms_ix_load, rdbms_add_ix, rdbms_unique_ix].

rdbms_basic(doc) ->
    [];
rdbms_basic(suite) ->
    [rdbms_create_table,
     rdbms_verify_write_basic, rdbms_types_undef, rdbms_types_list,
     rdbms_types_enum, rdbms_write_filter, rdbms_acl].
    
init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
    

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

install_schema(doc) ->
    [];
%%%    "delete existing schema (if any), create new schema, and start mnesia.";
install_schema(Config) when is_list(Config) ->
    ?line true = code:add_patha(?MNESIA_RECOMPILED),
    ?line true = code:add_patha(?MNESIA_PATCH_PATH),
    ?line true = code:add_patha(?RDBMS_PATH),
    ?line delete_schema(Config),
%%    ?line start_trace(),
    ?line mnesia:create_schema([node()]),
    ?line application:start(sasl),
    ?line mnesia:start([{access_module, rdbms},{debug,false}]),
    ok.

start_trace() ->
    dbg:tracer(),
    dbg:tpl(rdbms_index,[{'_',[],[{message,{return_trace}}]}]),
%%    dbg:tpl(mnesia_loader,[{'_',[],[{message,{return_trace}}]}]),
%%    dbg:tpl(mnesia_log,[{'_',[],[{message,{return_trace}}]}]),
%%    dbg:tpl(ets,insert,[{'_',[],[{message,{return_trace}}]}]),
%%    dbg:tpl(?MODULE,[{'_',[],[{message,{return_trace}}]}]),
    dbg:p(all,[c]).
%%    dbg:p(mnesia_tm,[p]).

delete_schema(doc) ->
    [];
%%%    "delete schema and stop mnesia.";
delete_schema(Config) when is_list(Config) ->
    ?line case application:get_application(mnesia) of
	      undefined ->
		  ok;
	      _Pid ->
		  ?line mnesia:stop()
	  end,
    ?line mnesia:delete_schema([node()]),
    ok.

basic_create_table(doc) ->
    [];
basic_create_table(Config) when is_list(Config) ->
    ?line {atomic, ok} = mnesia:create_table(basic, []),
    ?line chk_codegen(),
    ?line {atomic, ok} = mnesia:delete_table(basic),
    ?line chk_codegen(),
    ok.


rdbms_create_table(doc) ->
    "use rdbms:create_table() with a few rdbms attributes.";
rdbms_create_table(Config) when is_list(Config) ->
    io:format("rdbms_create_table()~n", []),
    ?line {atomic, ok} = rdbms:create_table(
			   rdbms_basic,
			   [{attributes, [a1,a2]},
			    {rdbms,
			     [{{attr, a1, type}, integer},
			      {{attr, a2, type}, string}
			     ]}
			    ]),
    ?line chk_codegen(),
    ok.

rdbms_verify_write_basic(doc) ->
    [];
rdbms_verify_write_basic(Config) when is_list(Config) ->
    io:format("rdbms_verify_write_basic()~n", []),
    ?line trans(fun() ->
			mnesia:write({rdbms_basic, 17, "foo"})
		end),
    ?line fail_trans(fun() ->
			     mnesia:write({rdbms_basic, 17, 17})
		     end),
    ?line fail_trans(fun() ->
			     mnesia:write({rdbms_basic, foo, "str"})
		     end).

rdbms_types_undef(doc) ->
    "test the 'undefined' type";
rdbms_types_undef(Config) when is_list(Config) ->
    ?line {atomic, ok} = rdbms:create_table(
			   rdbms_t_undef,
			   [{attributes, [key, value]},
			    {rdbms,
			     [{{attr,value,type}, {'or',
						   [integer,
						    undefined]}}
			     ]}
			    ]),
    ?line chk_codegen(),
%%%    ?line start_trace(),
    ?line trans(fun() ->
			?line mnesia:write({rdbms_t_undef,1,undefined}),
			?line mnesia:write({rdbms_t_undef,1,17})
		end),
    ?line fail_trans(fun() ->
			     ?line mnesia:write({rdbms_t_undef,1,foo})
		     end),
    ok.


rdbms_types_list(doc) ->
    "test the {list,T} type";
rdbms_types_list(Config) when is_list(Config) ->
    ?line {atomic, ok} = rdbms:create_table(
			   rdbms_t_list,
			   [{attributes, [key, value]},
			    {rdbms,
			     [{{attr,value,type}, {list,integer}}
			     ]}
			    ]),
    ?line chk_codegen(),
%%%    ?line start_trace(),
    ?line trans(fun() ->
			?line mnesia:write({rdbms_t_list,1,[]}),
			?line mnesia:write({rdbms_t_list,2,
					   lists:seq(1,7)})
		end),
    ?line fail_trans(fun() ->
			     ?line mnesia:write({rdbms_t_list,1,[1,2,a]})
		     end),
    ok.

rdbms_types_enum(doc) ->
    "test the {enum,T} type";
rdbms_types_enum(Config) when is_list(Config) ->
    ?line {atomic, ok} = rdbms:create_table(
			   rdbms_t_enum,
			   [{attributes, [key, value]},
			    {rdbms,
			     [{{attr,value,type}, {enum,[a,b,c]}}
			     ]}
			    ]),
    ?line chk_codegen(),
%%%    ?line start_trace(),
    ?line trans(fun() ->
			?line mnesia:write({rdbms_t_enum,1,a}),
			?line mnesia:write({rdbms_t_enum,1,b}),
			?line mnesia:write({rdbms_t_enum,2,c})
		end),
    ?line fail_trans(fun() ->
			     ?line mnesia:write({rdbms_t_enum,1,d})
		     end),
    ok.


rdbms_write_filter(doc) ->
    [];
rdbms_write_filter(Config) when is_list(Config) ->
    ?line {atomic, ok} = rdbms:create_table(
			   rdbms_w_filter,
			   [{attributes, [key, value]},
			    {rdbms,
			     [{write_filter, 
			       [{{rdbms_w_filter,'$1','$2'},
				 [], [{{rdbms_w_filter,'$1',{float,'$2'}}}]}]},
			      {{attr,value,type}, float}
			     ]}
			    ]),
     ?line {atomic, ok} = rdbms:create_table(
			   rdbms_no_w_filter,
			   [{attributes, [key, value]},
			    {rdbms,
			     [
			      {{attr,value,type}, float}
			     ]}
			    ]),
   
    ?line trans(fun() ->
			mnesia:write({rdbms_w_filter, 17, 1})
		end),
    ?line trans(fun() ->
			mnesia:write({rdbms_w_filter, 17, 1.0})
		end),
    ?line fail_trans(fun() ->
			mnesia:write({rdbms_no_w_filter, 17, 1})
		end),
    ?line trans(fun() ->
			mnesia:write({rdbms_no_w_filter, 17, 1.0})
		end).


rdbms_acl(doc) ->
    "test access control list";
rdbms_acl(Config) when is_list(Config) ->
%%%    ?line start_trace(),
    ?line {atomic,ok} = rdbms:create_table(
			  acl1, [{rdbms,
				  [
				   {acl, [{delete,false}]}
				  ]}]),
    ?line {atomic,ok} = rdbms:create_table(
			  acl2, [{rdbms,
				  [
				   {acl, [{read,{?MODULE,acl2_read}}]}
				  ]}]),
    ?line trans(fun() ->
			mnesia:write({acl1,1,a}),
			mnesia:write({acl2,1,a}),
			mnesia:write({acl2,2,b})
		end),
    ?line fail_trans(fun() ->
			     mnesia:delete({acl1,1})
		     end),
    ?line trans(fun() ->
			[] = mnesia:read({acl2,2})
		     end),
    ?line trans(fun() ->
			mnesia:read({acl2,1})
		end).
					  

acl2_read(Tab, Mode, {acl2,1}) ->
    true;
acl2_read(Tab, Mode, Other) ->
    false.

rdbms_refs_cascade(doc) ->
    [];
rdbms_refs_cascade(Config) when is_list(Config) ->
    ?line dbg:stop(),
    ?line {atomic,ok} = rdbms:create_table(
			  refs1_base,
			  [{attributes, [key, value]},
			   {rdbms, [{references,
				     [{attr, value, 
				       [{refs1_child, parent,
					 [{match, full},
					  {update, set_null},
					  {delete, cascade}]}]}
				     ]}
				   ]}
			  ]),
    ?line rdbms_codegen:output_src_file(),
    ?line {atomic,ok} = rdbms:create_table(
			  refs1_child,
			  [{attributes, [key, parent, value]}
			  ]),
%%%    ?line start_trace(),
    ?line trans(fun() ->
			mnesia:write({refs1_base, 1, a}),
			mnesia:write({refs1_child, 1, a, v1}),
			mnesia:write({refs1_child, 2, a, v2})
		end),
    ?line trans(fun() ->
			mnesia:delete({refs1_base, 1}),
			[] = mnesia:match_object({refs1_child,'_','_','_'})
		end),
    ok.

rdbms_refs_lookup(doc) ->
    ["Check referential integrity: deletes in the parent "
     "table are only allowed when there are no child objects, "
     "and inserts in the child table are only allowed if there is "
     "a parent object."];
rdbms_refs_lookup(Config) when is_list(Config) ->
    ?line {atomic,ok} = rdbms:create_table(
			  refs2_base,
			  [{attributes, [key, value]},
			   {rdbms, [{references,
				     [{attr, value, 
				       [{refs2_child, parent,
					 [{match, full},
					  {update, set_default},
					  {delete, no_action}]}]}
				     ]}
				   ]}
			  ]),
    ?line rdbms_codegen:output_src_file(),
    ?line {atomic,ok} = rdbms:create_table(
			  refs2_child,
			  [{attributes, [key, parent, value]},
			   {rdbms,
			    [{references,
			      [{attr, parent,
				[{refs2_base, value, 
				  [{match, full},
				   {update, no_action},
				   {delete, ignore}]}]}
			       ]}
			     ]}
			  ]),
    ?line trans(fun() ->
			mnesia:write({refs2_base, 1, a}),
			mnesia:write({refs2_child, 1, a, v1}),
			mnesia:write({refs2_child, 2, a, v2})
		end),
    ?line fail_trans(fun() ->
			     mnesia:write({refs2_child, 3, b, v3})
		     end),
%%%    ?line start_trace(),
    ?line fail_trans(fun() ->
			mnesia:delete({refs2_base, 1})
		     end),
    ?line trans(fun() ->
			mnesia:delete({refs2_child, 1}),
			mnesia:delete({refs2_child, 2}),
			mnesia:delete({refs2_base, 1})
		end),
    ok.

rdbms_ix(doc) ->
    [];
rdbms_ix(Config) when is_list(Config) ->
%%%    ?line start_trace(),
    ?line {atomic,ok} = rdbms:create_table(
			  ix1,
			  [{disc_copies, [node()]},
			   {attributes, [key, value]},
			   {rdbms, 
			    [
			     {indexes, 
			      [{{value,words},?MODULE,word_attr_ix,[],[]}]}
			    ]}
			  ]),
    write_ix1(),
    test_ix1(),
    OldLevel = mnesia:set_debug_level(trace),
    write_ix1_2(),
    mnesia:set_debug_level(OldLevel),
    test_ix1_2().

rdbms_ix_load(doc) ->
    "verify that index tables are loaded automatically at restart";
rdbms_ix_load(Config) when is_list(Config) ->
    ?line catch mnesia:stop(),
    ?line io:format("mnesia stopped~n", []),
%%%    ?line start_trace(),
    ?line mnesia:start([{access_module,rdbms},{debug,false}]),
    ?line io:format("mnesia started~n", []),
    test_ix1_2().

write_ix1() ->
    ?line mnesia:wait_for_tables([ix1], 3000),
    ?line Tab = ix1,
    O1 = {Tab,1,"a b c d"},
    O2 = {Tab,2,"a b"},
    O3 = {Tab,3,"b c d"},
    O4 = {Tab,4,"e f"},
    O5 = {Tab,5,"f g"},

    ?line trans(fun() ->
			lists:foreach(
			  fun(O) ->
				  mnesia:write(O)
			  end, [O1,O2,O3,O4,O5]),
			[O1,O2] =
			    lists:sort(
			      mnesia:index_read(Tab, "a", {value,words}))
		end).

test_ix1() ->
    ?line ok = mnesia:wait_for_tables([ix1], 30000),
    ?line Tab = ix1,
    O1 = {Tab,1,"a b c d"},
    O2 = {Tab,2,"a b"},
    O3 = {Tab,3,"b c d"},
    O4 = {Tab,4,"e f"},
    O5 = {Tab,5,"f g"},

    ?line trans(fun() ->
			[O1,O2,O3] =
			    lists:sort(
			      mnesia:index_read(Tab, "b", {value,words})),
			[O1,O3] =
			    lists:sort(
			      mnesia:index_read(Tab, "d", {value,words}))
		end).

write_ix1_2() ->
    ?line Tab = ix1,
    O1 = {Tab,1,"a b c d"},
    O2 = {Tab,2,"a b"},
    O3 = {Tab,3,"b c d"},
    O4 = {Tab,4,"e f"},
    O5 = {Tab,5,"f g"},
    O6 = {Tab,1,"c d"},
    ?line trans(fun() ->
			lists:foreach(
			  fun(Obj) ->
				  mnesia:write(Obj)
			  end, [O1,O2,O3,O4,O5,O6])
		end).


test_ix1_2() ->
    ?line ok = mnesia:wait_for_tables([ix1], 30000),
    ?line Tab = ix1,
    O1 = {Tab,1,"a b c d"},
    O2 = {Tab,2,"a b"},
    O3 = {Tab,3,"b c d"},
    O4 = {Tab,4,"e f"},
    O5 = {Tab,5,"f g"},
    O6 = {Tab,1,"c d"},
    ?line trans(fun() ->
			[O2,O3] =
			    lists:sort(
			      mnesia:index_read(Tab, "b", {value,words})),
			[O6,O3] =
			    lists:sort(
			      mnesia:index_read(Tab, "d", {value,words}))
		end),
    ok.
   


rdbms_add_ix(doc) ->
    "First creating a table and data, then creating an index";
rdbms_add_ix(Config) when is_list(Config) ->
    ?line {atomic,ok} = rdbms:create_table(
			  ix2,
			  [{attributes, [key, value]}]),
    ?line mnesia:wait_for_tables([ix2], 3000),
    ?line Tab = ix2,
    O1 = {Tab,1,"a b c d"},
    O2 = {Tab,2,"a b"},
    O3 = {Tab,3,"b c d"},
    O4 = {Tab,4,"e f"},
    O5 = {Tab,5,"f g"},

    ?line trans(fun() ->
			lists:foreach(
			  fun(O) ->
				  mnesia:write(O)
			  end, [O1,O2,O3,O4,O5])
		end),
    ?line mnesia_schema:schema_transaction(
	    fun() ->
		    rdbms_index:do_add_indexes(
		      ix2, [{{value,words},?MODULE,word_attr_ix,[],[]}])
	    end),
    ?line mnesia:wait_for_tables([ix2], 3000),
    ?line trans(fun() ->
			[O1,O2,O3] =
			    lists:sort(
			      mnesia:index_read(Tab, "b", {value,words})),
			[O1,O3] =
			    lists:sort(
			      mnesia:index_read(Tab, "d", {value,words}))
		end),
    ok.
    
rdbms_unique_ix(doc) ->
    "Test the 'unique' option for indexes";
rdbms_unique_ix(Config) when is_list(Config) ->
    ?line start_trace(),
    ?line {atomic,ok} = rdbms:create_table(
			  u_ix,
			  [{ram_copies, [node()]},
			   {attributes, [key, value]},
			   {rdbms, 
			    [
			     {indexes, 
			      [{{value,value},?MODULE,ix_value,[],
				[{type,set},
				 {unique, true}]}]}
			    ]}
			  ]),
    ?line trans(fun() ->
			mnesia:write({u_ix,1,a}),
			mnesia:write({u_ix,2,b})
		end),
    ?line trans(fun() ->
			[{u_ix,1,a}] = 
			    mnesia:index_read(u_ix,a,{value,value}),
			[{u_ix,2,b}] = 
			    mnesia:index_read(u_ix,b,{value,value})
		end),
    ?line fail_trans(fun() ->
			     mnesia:write({u_ix,3,a})
		     end).



chk_codegen() ->
    true = rdbms_codegen:last_codegen_successful().
	

trans(F) ->
    try mnesia:activity(transaction,
			fun() ->
				try F()
				catch
				    error:Reason ->
					erlang:error(
					  {Reason,
					   erlang:get_stacktrace()});
				      exit:Reason ->
					erlang:error(
					  {Reason,
					   erlang:get_stacktrace()})
				end
			end)
    catch
	error:Reason ->
	    io:format("FAILED!!! ~p, ~p~n", [Reason,
					     erlang:get_stacktrace()]),
	    erlang:error({Reason, erlang:get_stacktrace()});
	  exit:Reason ->
	    io:format("FAILED!!! ~p, ~p~n", [Reason,
					     erlang:get_stacktrace()]),
	    erlang:error({Reason, erlang:get_stacktrace()})
    end,
    ok.

fail_trans(F) ->
    try mnesia:activity(transaction, F) of
	Success ->
	    erlang:error({should_have_failed, Success})
	catch
	    error:Reason ->
		io:format("Failed with Reason = ~p (ok?)~n", [Reason]),
		ok;
	      exit:Reason ->
		io:format("Failed with Reason = ~p (ok?)~n", [Reason]),
		ok
	end.


word_attr_ix(Str, _) ->
    [to_lower(W) || W <- string:tokens(Str, " \t\rn")].

to_lower(Word) ->
    lists:map(fun(C) when $A =< C, C =< $Z ->
		      $a + (C - $A);
		 (C) ->
		      C
	      end, Word).
ix_value(V, _) ->
    [V].
