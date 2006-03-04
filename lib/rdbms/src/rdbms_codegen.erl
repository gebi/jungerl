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
%%% File    : rdbms_codegen.erl
%%% Author  : Ulf Wiger <etxuwig@ws12858>
%%% Description : Generates verification code for rdbms
%%%
%%% Created : 30 Sep 2005 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_codegen).


-export([tab/1, codegen/2, regenerate/0]).
-export([last_codegen_successful/0, get_forms/0]).
-export([generate_src_file/2, generate_src_file/3]).
-export([output_src_file/0, output_src_file/1]).

-include("rdbms.hrl").
-include("mnesia.hrl").
-define(MOD, rdbms_verify_jit).

regenerate() ->
    Tabs = mnesia:system_info(tables) -- [schema],
    %%
    case Tabs of
	[] ->
	    io:format("No tables defined -- no need to regenerate~n", []);
	[_|_] ->
	    io:format("~p: starting regenerate()~n", [statistics(wall_clock)]),
	    Dir = (catch mnesia:system_info(directory)),  % not used
	    io:format("Dir = ~p~n", [Dir]),
	    OutFile = objfile(Dir),
	    case generate_bin(Tabs, 1) of
		{{ok, ?MOD, Bin}, Forms} ->
		    io:format("~p: loading JIT module (diskless)~n", 
			      [statistics(wall_clock)]),
		    try begin
			    code:purge(?MOD),
			    {module, ?MOD} = 
				code:load_binary(?MOD, OutFile, Bin),
				mnesia_lib:set(rdbms_jit, {forms,Forms})
			end
		    catch
			error:Error ->
			    Info = {Error, erlang:get_stacktrace()},
			    mnesia_lib:set(rdbms_jit, {error, Info}),
			    erlang:error(Info)
		    end;
		{error, Reason} ->
			    Info = {Reason, erlang:get_stacktrace()},
			    mnesia_lib:set(rdbms_jit, {error, Info}),
			    erlang:error(Info)
	    end
    end.


last_codegen_successful() ->
    case ?catch_val(rdbms_jit) of
	{forms, _} ->
	    true;
	_ ->
	    false
    end.

get_forms() ->
    {forms, Forms} = ?catch_val(rdbms_jit),
    Forms.

	

tab(T) ->
    Info = rdbms_props:schema_table_info(T, all),
    RecName = proplists:get_value(record_name, Info),
    io:format("RecName = ~p~n", [RecName]),
    Attrs = proplists:get_value(attributes, Info),
    io:format("Attrs = ~p~n", [Attrs]),
    Vars = [{var,list_to_atom("V_" ++ atom_to_list(Attr))} || Attr <- Attrs],
    io:format("Vars = ~p~n", [Vars]),
    UserProps = proplists:get_value(user_properties, Info),
    %% There will be no type_rec if no types have been defined
    %% That also means: no type checks, obviously
    WFilter = proplists:get_value(write_filter, UserProps, no_filter),
    TChks = case proplists:get_value(rec_type, UserProps, no_type) of
		{tuple, _Arity, Types} ->
		    type_checks(Vars, tl(Types), Attrs);
		no_type ->
		    []
	    end,
    io:format("TChks = ~p~n", [TChks]),
    MatchRec = [RecName|Vars],
    {T, [{record_name, RecName},
	 {write_filter, WFilter},
	 {match_rec, MatchRec},
	 {type_checks, TChks}]}.


type_checks(Vars, Types, Attrs) ->
    L = lists:zip3(Vars, Types, Attrs),
    lists:foldr(
      fun({{var,V}, T, Attr}, Acc) ->
	      case T of
		  no_type -> Acc;
		  any -> Acc;
		  true -> Acc;
		  false -> [{V, false, Attr}|Acc];  % warn about this!!!
		  T -> [{V, T, Attr}|Acc]
	      end
      end, [], L).



callback_module(Tab) ->
    case rdbms_props:schema_table_info(Tab, frag_properties) of
	[] ->
	    mnesia;
	[_|_] ->
	    mnesia_frag
    end.
	      
get_opt(O, Opts) ->
    {value, {_,V}} = lists:keysearch(O, 1, Opts),
    V.


indexes(Tab) ->
    rdbms_props:schema_table_property(Tab, indexes, []).

references(Tab) ->
    rdbms_props:schema_table_property(Tab, references, []).

acl(Tab) ->
    case rdbms_props:schema_table_property(Tab, acl) of
	undefined ->
	    [];
	L when is_list(L) ->
	    L
    end.
    
%%% ==================================================
%%% Code generation
%%% ==================================================

src_file() ->
    "rdbms_verify_jit.erl" ++ stamp().

stamp() ->
    {I1,I2,I3} = erlang:now(),
    L = fun(I) -> integer_to_list(I) end,
    lists:concat([".",L(I1),".",L(I2),".",L(I3)]).
		 

%%% src_file(Dir) ->
%%%     filename:join(Dir, src_file()).


objfile() ->
    "rdbms_verify_jit" ++ code:objfile_extension().

objfile(Dir) ->
    filename:join(Dir, objfile()).

%%% absname(Dir) ->
%%%     filename:join(Dir, "rdbms_verify_jit").

generate_src_file(Tabs, L1) ->
    generate_src_file(src_file(), Tabs, L1).

generate_src_file(File, Tabs, L1) ->
    io:format("generating src file to ~p~n", [File]),
    [{attribute,_,file,_}|_] = Forms = codegen(Tabs, L1),
%%%    io:format("Forms = ~p~n", [Forms]),
    output_src_file(File, Forms).


output_src_file() ->
    output_src_file(src_file()).
output_src_file(File) ->
    case ?catch_val(rdbms_jit) of
	{'EXIT', _} ->
	    io:format(
	      "No forms in mnesia_gvar - generating from scratch...~n", []),
	    generate_src_file(File, mnesia:system_info(tables) -- [schema], 1);
	{forms, Forms} ->
	    output_src_file(File, Forms)
    end.

output_src_file(File, Forms) ->
    Out = [[erl_pp:form(F), "\n"] || F <- tl(Forms)],
    {ok, _Mod, _Bin} = compile:forms(Forms, [no_warnings]),
    {ok, Fd} = file:open(File, [write]),
    io:fwrite(Fd, "~s~n", [Out]),
    file:close(Fd).

generate_bin(Tabs, L1) ->
    [{attribute,_,file,_}|_] = Forms = codegen(Tabs, L1),
%%%    io:format("Forms = ~p~n", [Forms]),
    io:format("~p: forms generated~n", [statistics(wall_clock)]),
    SrcF = src_file(),
    SrcRes = (catch output_src_file(SrcF, Forms)),
    io:format("SrcRes (~p) = ~p~n", [SrcF, SrcRes]),
    Res = (catch compile:forms(Forms, [report_errors,report_warnings])),
    io:format("Res = ~p~n", [Res]),
    {Res, Forms}.


codegen(Tabs, L1) ->
    L2 = L1+2, L3 = L2+1,
    {IndexFun, L4} = codegen_indexes(Tabs, L3+2),
    {RefsFun, L5} = codegen_references(Tabs, L4+2),
    {TIFun, L6} = codegen_table_info(Tabs, L5+2),
    {ModFun, L7} = codegen_module(Tabs, L6+2),
    {AclFun, L8} = codegen_acl(Tabs, L7+2),
    {AccessFun, L9} = codegen_access(Tabs, L8+2),
    {OnReadFun, L10} = codegen_onread(Tabs, L9+2),
    {TabFuns, L11} = codegen_tabs(Tabs, L10+2),
    {AttrPropFuns, L12} = codegen_attr_props(Tabs, L11+2),
    {TabPropFuns, L13} = codegen_tab_props(Tabs, L12+2),
    [{attribute,L1, file, {"rdbms_verify_jit.erl",1}},
     {attribute,L2, module, rdbms_verify_jit},
     {attribute,L3, export, [{validate_rec, 2},
			     {indexes, 1},
			     {references, 1},
			     {acl, 1},
			     {verify_access, 3},
			     {on_read, 2},
			     {module, 1},
			     {table_info, 2},
			     {attr_property, 4},
			     {table_property, 3}]} |
     IndexFun
     ++ RefsFun
     ++ TIFun
     ++ ModFun
     ++ AclFun 
     ++ AccessFun
     ++ OnReadFun
     ++ TabFuns
     ++ AttrPropFuns
     ++ TabPropFuns
     ++ [{eof, L13+1}]].

codegen_indexes(Tabs, L1) ->
    {Body, L2} = lists:mapfoldl(
		   fun(Tab, L) ->
			   Indexes = indexes(Tab),
			   {{clause, L, [{atom, L, Tab}],
			     [],
			     [erl_parse:abstract(Indexes, L+1)]},
			    L+2}
		   end, L1+1, Tabs),
    {[{function, L1, indexes, 1, Body}], L2}.

codegen_references(Tabs, L1) ->
    {Body, L2} = lists:mapfoldl(
		   fun(Tab, L) ->
			   Refs = references(Tab),
			   {{clause, L, [{atom, L, Tab}],
			     [],
			     [erl_parse:abstract(Refs, L+1)]},
			    L+2}
		   end, L1+1, Tabs),
    {[{function, L1, references, 1, Body}], L2}.

codegen_module(Tabs, L1) ->
    {Body, L2} = lists:mapfoldl(
		   fun(Tab, L) ->
			   Mod = callback_module(Tab),
			   {{clause, L, [{atom, L, Tab}],
			     [],
			     [{atom, L+1, Mod}]},
			    L+2}
		   end, L1+1, Tabs),
    {[{function, L1, module, 1, Body}], L2}.

codegen_acl(Tabs, L1) ->
    {Body, L2} = lists:mapfoldl(
		   fun(Tab, L) ->
			   Acl = acl(Tab),
			   {{clause, L, [{atom, L, Tab}],
			     [],
			     [erl_parse:abstract(Acl, L+1)]},
			    L+2}
		   end, L1+1, Tabs),
    {[{function, L1, acl, 1, Body}], L2}.

codegen_access(Tabs, L0) ->
    F = fun(Tab, L) ->
		L1 = L+1, L2 = L1+1,
		case acl(Tab) of
		    [] ->
			{[{clause, L, [{atom, L, Tab},
				       {var, L, '_Rec'},
				       {var, L, '_Mode'}],
			   [],
			   [{atom, L+1, true}]}],
			 L+2};
		    [_|_] = Acl ->
			{[{clause, L, [{atom,L,Tab},
				       {var, L,'Rec'},
				       {var, L,'Mode'}],
			   [],
			   [{call,L1,{remote,L1,
				      {atom,L1,rdbms_verify},
				      {atom,L1,check_access}},
			     [gen_term(Acl,L2),
			      {var,L2,'Mode'},
			      {var,L2,'Rec'},
			      {atom,L2,Tab}]
			     }]
			   }], L2+2}
		end
	end,
    {Body, L3} = 
	lists:mapfoldl(F, L0, Tabs),
    {[{function, L3, verify_access, 3, lists:concat(Body)}], L3+2}.


%%% The generated function takes a 4th dummy attribute, _Default, in order
%%% be compatible with the rdbms_verify counterpart.
codegen_attr_props(Tabs, L1) ->
    {Body, L2} =
	lists:mapfoldl(
	  fun(Tab, L) ->
		  Attrs = rdbms_props:schema_table_info(Tab, attributes),
		  P1 = lists:foldl(
			 fun({{attr,A,P}, V}, Acc) ->
				 orddict:store({A,{atom,P}}, V, Acc);
			    (_, Acc) -> Acc
			 end, orddict:new(),
			 rdbms_props:schema_table_info(Tab, user_properties)),
		  %% att catch-all clauses. {var,...} will come after {atom,..}
		  %% in the orddict.
		  P2 = lists:foldl(
			 fun(A, Acc) -> 
				 orddict:store({A,{var,'_Prop'}},undefined,Acc)
			 end, P1, Attrs),
		  lists:mapfoldl(
		    fun({{AttrName, {Type,Prop}}, Value}, Lx) ->
			    {{clause, Lx, [{atom, Lx, Tab},
					   {atom, Lx, AttrName},
					   {Type, Lx, Prop},
					   {var,  Lx, '_Default'}],
			      [],
			      [erl_parse:abstract(Value, Lx+1)]},
			     Lx+2}
		    end, L, P2)
	  end, L1, Tabs),
    {[{function, L1, attr_property, 4, lists:concat(Body)}], L2}.


			   
%%% The generated function takes a 3rd dummy attribute, _Default, in order
%%% be compatible with the rdbms_verify counterpart.
codegen_tab_props(Tabs, L1) ->
    {Body, L2} =
	lists:mapfoldl(
	  fun(Tab, L) ->
		  Props = lists:foldl(
			    fun({references, V}, Acc) ->
				    orddict:store({term,references},V, Acc);
			       ({write_filter, V}, Acc) ->
				    orddict:store({term,write_filter},V,Acc);
			       ({read_filter, V}, Acc) ->
				    orddict:store({term,read_filter},V,Acc);
			       ({acl, V}, Acc) ->
				    orddict:store({term,acl},V, Acc);
			       ({{typedef,_}=T, V}, Acc) ->
				    orddict:store({term,T}, V, Acc);
			       (_, Acc) -> Acc
			    end, orddict:new(), rdbms_props:schema_table_info(
						  Tab, user_properties)),
		  lists:mapfoldl(
		    fun({{Type,PropName}, Value}, Lx) ->
			    PPat = case Type of
				       term -> gen_term(PropName, Lx);
				       var -> {var, Lx, PropName}
				   end,
			    {{clause, Lx, [{atom,Lx,Tab},
					   PPat,
					   {var, Lx,'_Default'}],
			      [],
			      [erl_parse:abstract(Value, Lx+1)]},
			     Lx+2}
		    end, L, Props ++ [{{var,'_PropName'}, undefined}])
	  end, L1, Tabs),
    {[{function, L1, table_property, 3, lists:concat(Body)}], L2}.

codegen_onread(Tabs, L0) ->
    SpecialTabs =
	lists:foldr(
	  fun(Tab, Acc) ->
		  Acl = acl(Tab),
		  ReadWhat = lists:foldr(fun({read,W},_) ->
						 W;
					    ({'_',W},_) ->
						 W;
					    (_, W) ->
						 W
					 end, true, Acl),
		  RFilter = rdbms_props:schema_table_property(
			      Tab,read_filter,no_filter),
		  case {ReadWhat, RFilter} of
		      {true, no_filter} ->
			  Acc;
		      _ ->
			  [{Tab, ReadWhat, RFilter}|Acc]
		  end
	  end, [], Tabs),
    {Body, L2} = 
	lists:mapfoldl(
	  fun({Tab, false, _}, Lx) ->
		  Lx1 = Lx+1,
		  {{clause, Lx, [{atom,Lx,Tab},
				{var,Lx,'_Objs'}],
		    [],
		    [{nil,Lx1}]}, Lx1+1};
	     ({Tab, true, RFilter}, Lx) ->
		  Lx1 = Lx+1, Lx2 = Lx1+1,
		  {{clause, Lx, [{atom,Lx,Tab},
				{var,Lx,'Objs'}],
		    [],
		    [{call,Lx1,{remote,Lx1,
				{atom,Lx1,rdbms_ms},{atom,Lx1,run_ms}},
		      [{var, Lx1, 'Objs'},
		       gen_term(RFilter,Lx2)]}
%%% 		     {call,Lx1, {remote,Lx1,
%%% 				{atom,Lx1,ets}, {atom,Lx1,match_spec_run}},
%%% 		      [{var,Lx1,'Objs'},
%%% 		       {call, Lx2, {remote,Lx2,
%%% 				   {atom,Lx2,ets},
%%% 				   {atom,Lx2,match_spec_compile}},
%%% 			[gen_term(RFilter,Lx2)]}
%%% 		      ]}
		    ]}, Lx2+1};
	     ({Tab, {M,F}, no_filter}, Lx) ->
		  Lx1 = Lx+1, Lx2 = Lx1+1, Lx3 = Lx2+1, Lx4 = Lx3+1,
		  {{clause, Lx, [{atom,Lx,Tab},
				{var, Lx,'Objs'}],
		    [],
		    [{call, Lx1, {remote,Lx1,
				 {atom,Lx1,lists},
				 {atom,Lx1,filter}},
		      [{'fun',Lx2,
			{clauses,
			 [{clause, Lx2,
			   [{var, Lx2, 'Obj'}],
			   [],
			   [{op,Lx3,'==',{atom,Lx3,true},
			     {call,Lx3,{remote,Lx3,
				       gen_term(M,Lx3),
				       gen_term(F,Lx3)},
			      [{atom,Lx3,Tab},
			       {atom,Lx3,read},
			       {var,Lx3,'Obj'}]}}
			    ]}
			  ]}
			},
		       {var, Lx4, 'Objs'}
		      ]}
		    ]}, Lx4+1};
	     ({Tab, {M,F}, RFilter}, Lx) ->
		  Lx1 = Lx+1, Lx2 = Lx1+1, Lx3 = Lx2+1,
		  Lx4 = Lx3+1, Lx5 = Lx4+1,
		  {{clause, Lx, [{atom,Lx,Tab},
				 {var, Lx,'Objs'}],
		    [],
		    [{call, Lx1, {remote,Lx1,
				  {atom,Lx1,lists},
				  {atom,Lx1,filter}},
		      [{'fun',Lx2,
			{clauses,
			 [{clause, Lx2,
			   [{var, Lx2, 'Obj'}],
			   [],
			   [{op,Lx3,'==',{atom,Lx3,true},
			     {call,Lx3,{remote,Lx3,
					gen_term(M,Lx3),
					gen_term(F,Lx3)},
			      [{atom,Lx3,Tab},
			       {atom,Lx3,read},
			       {var,Lx3,'Obj'}]}}
			    ]}
			  ]}
			},
		       {call,Lx4,{remote,Lx4,
				  {atom,Lx4,rdbms_ms},{atom,Lx4,run_ms}},
			[{var, Lx4, 'Objs'},
			 gen_term(RFilter,Lx5)]}
%%% 		       {call,Lx4,{remote,Lx4,
%%% 				  {atom,Lx4,ets},{atom,Lx4,match_spec_run}},
%%% 			[{var, Lx4, 'Objs'},
%%% 			 {call,Lx5,{remote,Lx5,
%%% 				    {atom,Lx5,ets},
%%% 				    {atom,Lx5,match_spec_compile}},
%%% 			  [gen_term(RFilter,Lx5)]}
%%% 			 ]}
		      ]}
		    ]}, Lx5+1}
	  end, L0, SpecialTabs),
    io:format("Body = ~p~n", [Body]),
    L3 = L2+1, L4 = L3+1,
    Body1 = Body ++ 
	[{clause,L3, [{var,L3,'_'},{var,L3,'Objs'}],
	  [],
	  [{var,L4,'Objs'}]}],
    {[{function, L0, on_read, 2, Body1}], L4+2}.


codegen_table_info(_Tabs, L1) ->
    L2 = L1+1, L3=L2+1, L4=L3+1,
    {[{function, L1, table_info, 2,
       [{clause, L1, [{var,L1,'Tab'},
		      {var,L1,'Item'}], [],
	 [{match, L2, {tuple, L2, [{var,L2,'_'},
				   {var,L2,'Tid'},
				   {var,L2,'Ts'}]},
	   {call, L2, {atom,L2,get}, [{atom, L2, mnesia_activity_state}]}},
	  {match, L3, {var,L3,'Mod'},
	   {call, L3, {atom,L3,module}, [{var,L3,'Tab'}]}},
	  {call, L4, {remote, L4, {var,L4,'Mod'}, {atom,L4,table_info}},
	   [{var,L4,'Tid'}, {var,L4,'Ts'}, {var,L4,'Tab'}, {var,L4,'Item'}]}
	 ]}]}], L4}.

codegen_tabs(Tabs, L1) ->
    Tabs1 = lists:map(
	      fun(T) ->
		      FName = list_to_atom("validate_" ++ atom_to_list(T)),
		      Info = tab(T),
		      {T, FName, Info}
	      end, Tabs),
    L2 = L1+2, L3 = L2+1, L4 = L3+1,
    {Body, L5} = lists:mapfoldl(
		   fun({Tab, FName, _}, L) ->
			   {{clause, L, [{atom, L, Tab},
					 {var, L, 'Object'}],
			     [],
			     [{call, L, {atom, L, FName}, [{var,L,'Object'}]}]
			    }, L+1}
		   end, L4, Tabs1),
    {TabFuns, L6} = 
	 lists:mapfoldl(
	   fun({_Tab, FName, Info}, L) ->
		   codegen_tab(Info, FName, L)
	   end, L5+2, Tabs1),
    {[{function, L4, validate_rec, 2, Body}|
      TabFuns], L6}.


codegen_tab({Tab, Opts}, FName, L) ->
    L1 = L+1,
    MatchRec = get_opt(match_rec, Opts),
    WFilter = get_opt(write_filter, Opts),
    TypeChecks = get_opt(type_checks, Opts),
    MatchTuple = 
	{tuple, L, [{atom,L,hd(MatchRec)}|
		    lists:map(
		      fun({var,V}) ->
			      case lists:keymember(V, 1, TypeChecks) of
				  true -> {var, L, V};
				  false -> {var, L, mark_unused(V)}
			      end
		      end, tl(MatchRec))]},
    HdClause =
 	if WFilter == no_filter ->
		[MatchTuple];
 	   true ->
		[{match, L, wild_record(MatchTuple), {var,L, 'Obj'}}]
	end,
    TypeCode =
	fun(Lx) ->
		codegen_typechecks(TypeChecks, Tab, Lx)
	end,
    {Code, L2} =
	if WFilter == no_filter ->
		TypeCode(L1);
	   true ->
		codegen_filtercode(WFilter, Tab, MatchTuple, TypeCode, L1)
	end,
    L3 = L2+1, L4 = L3+1, L5 = L4+1,
    {{function, L, FName, 1,
      [{clause, L, HdClause,
	[],
	Code ++ [{atom,L2,true}]
       },
       {clause, L3, [{var,L3,'_'}], [], 
	gen_error(bad_record, L4, {val, Tab}, {val, MatchRec})}
      ]}, L5}.

codegen_filtercode(Filter, Tab, MatchTuple, TypeGen, L) ->
    L1 = L+1, L2 = L1+1, L3 = L2+1, L4 = L3+1, 
    FilterAbst = gen_term(Filter, L1),
    {TypeCode,L5} = TypeGen(L4),
    L6 = L5+1, L7 = L6+1,
    {[{match, L1,
       {var,L1,'Obj1'},
       {call, L1, {remote, L1, {atom,L1,rdbms_verify}, {atom,L1,filter_obj}},
	[{var,L1,'Obj'},
	 FilterAbst,
	 {cons, L1, gen_term(Tab,L1),
	  {cons, L1, FilterAbst,
	   {nil,L1}}}]}},
      {'case', L2, {var,L2,'Obj1'},
       [{clause, L3, [MatchTuple],
	 [],
	 TypeCode},
	{clause, L5, [{var,L5,'_'}],
	 [],
	 [{call, L6, {remote, L6, {atom,L6,erlang}, {atom,L6,error}},
	   [{tuple,L6,[{atom,L6,bad_filter_output},
		       {var,L6,'Obj1'}]
	    }]
	  }]}
       ]}
     ], L7}.

codegen_typechecks(Ts, Tab, L) ->
    case lists:mapfoldl(
	   fun({Var, Type, Attr}, L1) ->
		   {{call, L1, {remote, L1,
				{atom, L1, rdbms_verify},
				{atom, L, check_type}},
		     [gen_term(Type, L1),
		      {var,L1,Var},
		      gen_term([Tab,Attr], L1)]},
		    L1+1}
	   end, L, Ts) of
	{[], L1} ->
	    {[{atom,L,true}], L1};
	Other ->
	    Other
    end.

wild_record({tuple,L,[Tag|Vs]}) ->
    {tuple,L,[Tag | [{var,L,'_'} || _ <- Vs]]}.

mark_unused(V) ->
    list_to_atom("_" ++ atom_to_list(V)).

%%% codegen_typechecks({Guards, Fns}, L) ->
%%%     {GCode, L1} = codegen_guards(Guards, type_error, L),
%%%     {FCode, L2} = codegen_funs(Fns, L1),
%%%     {GCode ++ FCode, L2}.


%%% codegen_valuechecks({Guards, Fns}, L) ->
%%%     {GCode, L1} = codegen_guards(Guards, value_error, L),
%%%     {FCode, L2} = codegen_funs(Fns, L1),
%%%     {GCode ++ FCode, L2}.
    

%%% gen_support_fns(validate_number, L) ->
%%%     L1 = L+1, L2 = L+2, L3 = L+3,
%%%     {{function, L, validate_number, 2,
%%%       [{clause, L, [{var, L, 'N'},
%%% 		    {var, L, 'Attr'},
%%% 		    {var, L, 'Expected'}],
%%% 	[[{call, L, {atom, L, is_integer}, [{var, L, 'N'}]}],
%%% 	 [{call, L, {atom, L, is_float}, [{var, L, 'N'}]}]],
%%% 	[{atom, L1, true}]},
%%%        {clause, L2, [{var, L2, '_N'},
%%% 		     {var, L2, 'Attr'}],
%%% 	[],
%%% 	gen_error(type_error, L3, {var, 'Attr'}, {var, 'Expected'})
%%%        }]
%%%      }, L3};
%%% gen_support_fns(validate_string, L) ->
%%%     L1 = L+1, L2 = L+2, L3 = L+3, L4 = L+4, L5 = L+5,
%%%     {{function, L, validate_string, 3,
%%%       [{clause, L,
%%% 	[{var,L,'S'},{var,L,'Attr'}, {var,L,'Expected'}],
%%% 	[],
%%% 	[{'try', L1,
%%% 	  [{call, L1, {atom,L1,list_to_binary}, [{var,L1,'S'}]}],
%%% 	  [{clause, L2, [{var,L2,'_'}],
%%% 	    [],
%%% 	    [{atom,L3,true}]}],
%%% 	  [{clause, L4, [{tuple, L4, [{atom,L4,error},
%%% 				      {var,L4,'_'},
%%% 				      {var,L4,'_'}]}],
%%% 	    [],
%%% 	    gen_error(type_error, L5, {var, 'Attr'}, {var, 'Expected'})}
%%% 	  ],
%%% 	  []
%%% 	 }]
%%%        }]
%%%      }, L5};
%%% gen_support_fns(validate_oid, L) ->
%%%     L1 = L+1, L2 = L+2, L3 = L+3, L4 = L+4,
%%%     {{function, L, validate_oid, 3,
%%%       [{clause, L, [{tuple, L, [{var, L, 'Node'},
%%% 				{tuple, L, [{var, L, 'Ms'},
%%% 					    {var, L, 'S'},
%%% 					    {var, L, 'Us'}]}
%%% 			       ]},
%%% 		    {var, L, 'Attr'},
%%% 		    {var, L, 'Expected'}],
%%% 	[[{call, L1, {atom, L1, is_atom}, [{var, L1, 'Node'}]},
%%% 	  {call, L2, {atom, L2, is_integer}, [{var, L2, 'Ms'}]},
%%% 	  {call, L3, {atom, L3, is_integer}, [{var, L3, 'S'}]},
%%% 	  {call, L4, {atom, L4, is_integer}, [{var, L4, 'Us'}]}
%%% 	 ]],
%%% 	[{atom, L+1, true}]},
%%%        {clause, L2, [{var, L2, '_Oid'},
%%% 		     {var, L2, 'Attr'},
%%% 		     {var, L2, 'Expected'}],
%%% 	[],
%%% 	gen_error(type_error, L3, {var, 'Attr'}, {var, 'Expected'})
%%%        }]
%%%      }, L3};
%%% gen_support_fns(bounds_incl, L) ->
%%%     gen_bounds(bounds_incl, '>=', '=<', L);
%%% gen_support_fns(bounds_excl, L) ->
%%%     gen_bounds(bounds_excl, '>=', '=<', L).

%%% gen_bounds(F, Op1, Op2, L) ->
%%%     L2 = L+2, L3 = L+3,
%%%     {{function, L, F, 4,
%%%       [{clause, L, [{var, L, 'Value'},
%%% 		    {var, L, 'Min'},
%%% 		    {var, L, 'Max'},
%%% 		    {var, L, 'Attr'},
%%% 		    {var, L, 'Expected'}],
%%% 	[[{op, L, Op1, {var, L, 'Min'}, {var, L, 'Value'}},
%%% 	  {op, L, Op2, {var, L, 'Value'}, {var, L, 'Max'}}]],
%%% 	[{atom, L+1, true}]},
%%%        {clause, L2, [{var, L2, '_Value'},
%%% 		     {var, L2, '_Min'},
%%% 		     {var, L2, 'Max'},
%%% 		     {var, L2, 'Attr'}],
%%% 	[],
%%% 	gen_error(out_of_bounds, L3, {var, 'Attr'}, {var, 'Expected'})
%%%        }]
%%%      }, L3}.

gen_error(_Error, L, Attr, Expected) ->
    [{call, L, {atom, L, throw},
      [{tuple, L, [{atom, L, type_error},
		   expand_arg(Attr, L),
		   {tuple, L, [{atom, L, expected},
			       expand_arg(Expected, L)]}
		  ]}
      ]}
    ].


%%% expand_guards(Gs, L) ->
%%%     [lists:map(
%%%        fun({Op, F, Left, Right}) ->
%%% 	       {Op, L, F, expand_arg(Left,L), expand_arg(Right,L)};
%%% 	  ({Op, F, Args}) ->
%%% 	       {Op, L, {atom, L, F}, expand_args(Args, L)}
%%%        end, Exprs) || Exprs <- Gs].



%%% expand_args(Args, L) ->
%%%     lists:map(
%%%       fun({var, V}) -> {var, L, V};
%%% 	 ({val, V}) -> gen_term(V, L)
%%%       end, Args).

expand_arg({var, V}, L) ->
    {var, L, V};
expand_arg({val, V}, L) ->
    gen_term(V, L).



gen_term(A, L) ->
    erl_parse:abstract(A, L).

