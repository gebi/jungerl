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
%%% The Original Code is plain_fsm-0.7.
%%%
%%% The Initial Developer of the Original Code is Ericsson AB
%%% AB. Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%%% All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%-------------------------------------------------------------------
%%% File    : plain_fsm.erl
%%% @doc Parse transform utility for plain_fsm
%%% @author Ulf Wiger, <ulf.wiger@ericsson.com>
%%% @end
%%% Created : 29 Jan 2004 by Ulf Wiger <ulf.wiger@ericsson.com>
%%%-------------------------------------------------------------------
-module(plain_fsm_xform).


-export([parse_transform/2,
	 format_error/1]).

-record(context, {module,
		  function,
		  arity}).

-define(PLAIN_FSM, plain_fsm).

-define(ERROR(R, T, F, I),
	begin
	    rpt_error(R, T, F, I),
	    
	    throw({error,erl_syntax:get_pos(
			   proplists:get_value(form,I)),{unknown,R}})
	end).

parse_transform(Forms, _Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try begin
	    {NewTree, _} = xform_plainfsm(Forms),
	    maybe_add_vsn_f(
	      [erl_syntax:revert(T) || T <- lists:flatten(NewTree)])
	end
    catch
	throw:{error,Ln,What} ->
	    {error, [{File, [{Ln,?MODULE,What}]}], []}
    end.


format_error({bad_arity,_Arity}) ->
    "Enclosing function for extended_receive must be of arity 1.";
format_error(Other) ->
    lists:flatten(
      io_lib:format("unknown error in parse_transform: ~p", [Other])).



xform_plainfsm(Forms) ->
    Bef = fun(function, Form, Ctxt) ->
		  {Fun, Arity} = erl_syntax_lib:analyze_function(Form),
		  {Form, Ctxt#context{function = Fun,
				      arity = Arity}, false};
	     (_, Form, Context) ->
		  {Form, Context, false}
	  end,
    Aft = fun(application, Form, Context, _SubAcc, Acc) ->
		  case erl_syntax_lib:analyze_application(Form) of
		      {?PLAIN_FSM, {extended_receive, 1}} ->
			  case Context#context.arity of
			      1 ->
				  Fname = Context#context.function,
				  handle_extended_recv(Form, Fname, Acc);
			      Other ->
				  throw({error,erl_syntax:get_pos(Form),
					 {bad_arity,Other}})
			  end;
		      {?PLAIN_FSM, {hibernate, 3}} ->
			  #context{module = Module} = Context,
			  {hibernate(Module, Form), Acc};
		      _ ->
			  {Form, Acc}
		  end;
	     (clause, Form, _Context, true, _Acc) ->
		  {erl_syntax:add_ann(bind_state, Form), true};
	     (function, Form, _Context, true, Acc) ->
		  {Form1, _} =
		      erl_syntax_lib:mapfold_subtrees(
			fun(Clause, Acc1) ->
				Anns = erl_syntax:get_ann(Clause),
				case lists:member(bind_state, Anns) of
				    true ->
					[Pat] = erl_syntax:clause_patterns(
						  Clause),
					CBod = erl_syntax:clause_body(Clause),
					CGd = erl_syntax:clause_guard(Clause),
					Clause1 =
					    erl_syntax:clause(
					      [erl_syntax:match_expr(
						 erl_syntax:variable(
						   '__FSM_State'),
						 Pat)],
					      CGd,
					      CBod),
					{Clause1, Acc1};
				    false ->
					{Clause, Acc1}
				end
			end, ok, Form),
		  {Form1, Acc};
	     (_, Form, _Context, _SubAcc, Acc) ->
		  {Form, Acc}
	  end,
    [Module] = [M || {attribute, _, module, M} <- Forms],
    transform(Forms, Bef, Aft, #context{module = Module}, []).


transform(Forms, Before, After, Context, Acc) ->
    F1 =
	fun(Form, Acc0) ->
		Type = erl_syntax:type(Form),
		{Form1, Context1, InitSubAcc} =
		    try Before(Type, Form, Context)
		    catch
			error:Reason ->
			    ?ERROR(Reason, 'before', Before, 
				   [{type, Type},
				    {context, Context},
				    {acc, Acc},
				    {form, Form}])
		    end,
		{Form2, SubAcc2} =
		    case erl_syntax:subtrees(Form1) of
			[] ->
			    {Form1, InitSubAcc};
			List ->
			    {NewList, NewSubAcc} =
				transform(
				  List, Before, After, Context1, InitSubAcc),
			    NewForm = erl_syntax:update_tree(Form, NewList),
			    {NewForm, NewSubAcc}
		    end,
		Type2 = erl_syntax:type(Form2),
		try After(Type2, Form2, Context, SubAcc2, Acc0)
		catch
		    error:Reason2 ->
			?ERROR(Reason2, 'after', After, 
			       [{type, Type2},
				{context, Context},
				{sub_acc, SubAcc2},
				{acc, Acc0},
				{form, Form2}])
		end
	end,
    F2 = fun(List, St) when is_list(List) ->
		 mapfoldl(F1, St, List);
	    (Form, St) ->
		 F1(Form, St)
	 end,
    mapfoldl(F2, Acc, Forms).

%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
mapfoldl(F, Accu0, [Hd|Tail]) ->
    {Before, Res, After, Accu1} =
	case F(Hd, Accu0) of
	    {Be, _, Af, _} = Result when is_list(Be), is_list(Af) ->
		Result;
	    {R1, A1} ->
		{[], R1, [], A1}
	end,
    {Rs, Accu2} = mapfoldl(F, Accu1, Tail),
    {Before ++ [Res| After ++ Rs], Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[], Accu}.



rpt_error(Reason, BeforeOrAfter, Fun, Info) ->
    Fmt = lists:flatten(
	    ["*** ERROR in parse_transform function:~n"
	     "*** Reason     = ~p~n"
	     "*** applying ~w fun (~p)~n",
	     ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, BeforeOrAfter, Fun | 
	    lists:foldr(
	      fun({K,V}, Acc) ->
		      [K, V | Acc]
	      end, [], Info)],
    io:format(Fmt, Args).


handle_extended_recv(Form, Fname, _Acc) ->
    [Arg] = erl_syntax:application_arguments(Form),
    {[get_parent_expr()], extended_recv(Arg, Fname), [], true}.

extended_recv(Arg, Fname) ->
    case erl_syntax:type(Arg) of
	receive_expr ->
	    Clauses = erl_syntax:receive_expr_clauses(Arg),
	    Timeout = erl_syntax:receive_expr_timeout(Arg),
	    Action = erl_syntax:receive_expr_action(Arg),
	    Clauses1 = extend_recv(Clauses, erl_syntax:atom(Fname)),
	    erl_syntax:receive_expr(Clauses1, Timeout, Action);
	_ ->
	    throw(illegal_argument)
    end.
    



get_parent_expr() ->
    {match,0,
     {var,0,'__FSM_Parent'},
     {call,0,{remote,0,
	      {atom,0,?PLAIN_FSM},
	      {atom,0,info}},
      [{atom,0,parent}]}}.


extend_recv(Clauses, Cont) ->
    [erl_syntax:clause(
       [erl_syntax:tuple([erl_syntax:atom('EXIT'),
			  erl_syntax:variable('__FSM_Parent'),
			  erl_syntax:variable('__FSM_Reason')])],
       [],
       [erl_syntax:application(
	  erl_syntax:atom(?PLAIN_FSM),
	  erl_syntax:atom(parent_EXIT),
	  [erl_syntax:variable('__FSM_Reason'),
	   erl_syntax:variable('__FSM_State')])]),
     erl_syntax:clause(
       [erl_syntax:tuple([erl_syntax:atom(system),
			  erl_syntax:variable('__FSM_From'),
			  erl_syntax:variable('__FSM_Req')])],
       [],
       [erl_syntax:application(
	  erl_syntax:atom(?PLAIN_FSM),
	  erl_syntax:atom(handle_system_msg),
	  [erl_syntax:variable('__FSM_Req'),
	   erl_syntax:variable('__FSM_From'),
	   erl_syntax:variable('__FSM_State'),
	   erl_syntax:fun_expr(
	    [erl_syntax:clause(
	       [erl_syntax:variable('__FSM_Sx')],
	       [],
	       [erl_syntax:application(
		  Cont,
		  [erl_syntax:variable('__FSM_Sx')])])])])
       ]) | Clauses].


hibernate(Module, Form) ->
    [M, F, A] = erl_syntax:application_arguments(Form),
    erl_syntax:application(
      erl_syntax:atom(erlang),
      erl_syntax:atom(hibernate),
      [erl_syntax:atom(?PLAIN_FSM),
       erl_syntax:atom(wake_up),
       erl_syntax:list(
	 [erl_syntax:application(erl_syntax:atom(data_vsn), []),
	  erl_syntax:atom(Module),
	  M, F, A])]).


%%% This function hasn't yet been re-written to use syntax_tools.
%%% OTOH, it is pretty stable as it is, as it operates only on the top
%%% level of the form list.
%%%
maybe_add_vsn_f(Forms) ->
    {Pre, Fns} = lists:splitwith(
                   fun(F) when tuple(F), element(1,F) == function ->
                           false;
                      (_) ->
                           true
                   end, Forms),
    Line = element(2,hd(lists:reverse(Pre))),
    %%
    Pre1 = case is_exported(data_vsn, 0, Forms) of
               true ->
                   Pre;
               false ->
                   Pre ++ [{attribute,Line,export,[{data_vsn,0}]}]
           end,
    FunExists = 
        lists:any(fun({function,_Line,data_vsn,0,_Clauses}) ->
                          true;
                     (_) ->
                          false
                  end, Fns),
    Fns1 = 
        case FunExists of
            true ->
                Fns;
            false ->
                [{eof,LastLine}|RevFns] = lists:reverse(Fns),
                lists:reverse(
		  [{eof,LastLine+2},
		   {function,LastLine,data_vsn,0,
		    [{clause,LastLine,[],[],[{integer,LastLine+1,0}]}]}
		   | RevFns])
        end,
    Pre1 ++ Fns1.

is_exported(Fun, Arity, Forms) ->
    lists:any(fun({attribute,_,compile,export_all}) ->
                      true;
                 ({attribute,_,export,Exports}) ->
                      lists:member({Fun,Arity}, Exports);
                 (_) ->
                      false
              end, Forms).
