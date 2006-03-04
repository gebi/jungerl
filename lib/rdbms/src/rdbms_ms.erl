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
%%% File    : rdbms_ms.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : Converts regexps to match expressions
%%%
%%% Created : 20 Jan 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_ms).

-compile(export_all).

-define(LIMIT, 255).
-define(TOTAL, {?MODULE,total}).
-define(TOTAL_LIMIT, 100000).


re(Re) ->
    re(Re, ?LIMIT).

%%% Limit signifies the maximum amount of lookahead
re(Re, Limit) ->
    case regexp:parse(Re) of
	{ok, PRe} ->
	    to_ms(PRe, 1, Limit);
	{error, _} = Error ->
	    Error
    end.

re_match(Objs, Re) ->
    re_match(Objs, Re, ?LIMIT).

re_match(Objs, Re, Limit) ->
    Ms = re(Re, Limit),
    MsC = ets:match_spec_compile(Ms),
    ets:match_spec_run(Objs, MsC).

sh_match(Objs, Sh) ->
    sh_match(Objs, Sh, ?LIMIT).

sh_match(Objs, Sh, Limit) ->
    Ms = re(regexp:sh_to_awk(Sh), Limit),
    MsC = ets:match_spec_compile(Ms),
    ets:match_spec_run(Objs, MsC).
    


to_ms(Re, NextVar, Limit) ->
    ReC = concat(Re),
    Str = to_str(Re, NextVar),
    UsedVars = used_vars(Str),
    VarExprs = lists:dropwhile(fun(X) -> is_integer(X) end, ReC),
    Gs = guards(VarExprs),
    initialize_total(),
    try Gs({UsedVars, Limit}) of
	GuardExpr ->
	    [{Str, [GuardExpr], ['$_']}]
    catch
	error:Reason ->
	    {error, {Reason, erlang:get_stacktrace()}}
    after
	cleanup()
    end.

to_str(Re, NV) ->
    to_str(Re, NV, fun(_) -> [] end).

to_str(I, NV, Tail) when is_integer(I) ->
    [I|Tail(NV)];
to_str(X, NV, Tail) when X==bos; X==eos ->
    %% skip
    Tail(NV);
to_str({kclosure, _}, NV, _Tail) ->
    next_var(NV);
to_str({pclosure, _}, NV, _Tail) ->
    [next_var(NV)|next_var(NV+1)];
to_str({Op,_Chars}, NV, Tail) when Op==char_class; Op==comp_class ->
    [next_var(NV)|Tail(NV+1)];
to_str({concat, A, B}, NextVar, Tail) ->
    to_str(A, NextVar, fun(NV1) ->
			       to_str(B, NV1, Tail)
		       end).

concat(Re) ->
    concat(Re, fun() -> [] end).

concat({concat, A, B}, Tail) ->
    concat(A, fun() ->
		      concat(B, Tail)
	      end);
concat(bos, Tail) ->
    Tail();
concat({Op, Closure}, Tail) when Op==kclosure; Op==pclosure ->
    [if is_integer(Closure) ->
	     {Op, Closure};
	true ->
	     {Op, concat(Closure)}
     end | Tail()];
concat(X, Tail) ->
    [X|Tail()].


%%% ================================================
%%% Variable handling

next_var(N) ->
    list_to_atom("$" ++ integer_to_list(N)).

used_vars(L) -> 
    used_vars(L, 1).

used_vars(V, P) when is_atom(V) ->
    [{{list,V},P}];
used_vars([], _) ->
    [];
used_vars([H|T], P) when is_atom(H) ->
    [{H, P}|used_vars(T, P+1)];
used_vars([_|T], P) ->
    used_vars(T, P+1).


cur({[{V,_}|_],_}) ->
    V;
cur({{V, _}, _}) when is_tuple(V) ->
    V.

next({[{{list,L},P}], Limit}) ->
    {{{'tl', L}, P+1}, Limit};
next({[_|Rest], Limit}) ->
    {Rest,Limit};
next({{V,P}, Limit}) when is_tuple(V), P < Limit ->
    incr_total(),
    {{{'tl', V},P+1}, Limit};
next(_) ->
    nomore.

initialize_total() ->
    put(?TOTAL, 0).

cleanup() ->
    erase(?TOTAL).

incr_total() ->
    Key = ?TOTAL,
    Prev = get(Key),
    case Prev+1 of
    	NewTot when NewTot > ?TOTAL_LIMIT ->
	    erlang:error(too_complex);
	NewTot ->
	    put(Key, NewTot)
    end.

next_guard(Next, Vs) ->
    case next(Vs) of
	nomore ->
	    false;
	Vs1 ->
	    Next(Vs1)
    end.


char_var({list,L}) ->
    {'hd',L};
char_var({'tl',_}=V) ->
    {'hd',V};
char_var(A) when is_atom(A) ->
    A.

list_var({list,V}) ->
    V;
list_var(V) ->
    V.

%%% End variable handling
%%% ================================================


kclosure(Closure, Next) ->
    FCl = closure(Closure),
    fun(Vs) ->
	    Test = fun(Vars, T1) ->
			   Recurse = fun() ->
					     case next(Vars) of
						 nomore ->
						     false;
						 Vars1 ->
						     T1(Vars1, T1)
					     end
				     end,
			   {'orelse',
			    {'andalso',
			     FCl(Vars),
			     Recurse()},
			    Next(Vars)}
		   end,
	    Test(Vs, Test)
    end.


pclosure(Closure, Next) ->       
    KClosure = kclosure(Closure, Next),
    FCl = closure(Closure),
    fun(Vs) ->
	    case next(Vs) of
		nomore ->
		    false;
		Vs1 ->
		    {'andalso',
		     FCl(Vs),
		     KClosure(Vs1)}
	    end
    end.

closure(Closure) ->
    if is_integer(Closure) ->
	    fun(Vs) ->
		    Cur = cur(Vs),
		    V = char_var(Cur),
		    {'andalso',
		     {'=/=', list_var(Cur), []},
		     {'==', V, Closure}}
	    end;
       is_list(Closure) ->
	    guards(Closure)
    end.



%%% ------------------------------------------------------------
%%% Erlang-based version of ets:match_spec_run/2

run_ms(Objs, Ms) ->
    lists:foldr(
      fun(Obj, Acc) ->
	      case 
		  lists:foldl(
		    fun({MatchHead, Gs, Prod},Acc1) ->
			    case match_head(Obj, MatchHead) of
				{true, Vars} ->
				    case match_guards(Gs, Vars) of
					true ->
					    {true, prod(Prod, Obj, Vars)};
					false ->
					    Acc1
				    end;
				false ->
				    Acc1
			    end
		    end, false, Ms) of
		  {true, Res} ->
		      [Res|Acc];
		  false ->
		      Acc
	      end
      end, [], Objs).

match_head(Obj, Pat) ->
    try match_head(Obj, Pat, []) of
	Vars when is_list(Vars) ->
	    {true, Vars}
    catch
	throw:false ->
	    false
    end.

match_head(Obj, Pat, Acc) when is_atom(Pat) ->
    case is_var(Pat) of
	true ->
	     [{Pat, Obj}|Acc];
	false ->
	    if Obj == Pat -> Acc;
	       true -> throw(false)
	    end
    end;
match_head({},{}, Acc) -> Acc;
match_head([],[], Acc) -> Acc;
match_head(Obj, Pat, Acc) 
  when is_tuple(Obj), is_tuple(Pat), size(Obj) == size(Pat) ->
    Sz = size(Pat),
    match_head_tuple(Obj,Pat,1,Sz,Acc);
match_head(Obj, Pat, Acc) when is_list(Obj) ->
    match_head_list(Obj, Pat, Acc).

match_head_tuple(Obj,Pat,Pos,Sz,Acc) when Pos =< Sz ->
    match_head_tuple(Obj,Pat,Pos+1,Sz,
		     match_head(element(Pos,Obj), element(Pos,Pat),Acc));
match_head_tuple(_, _, _, _, Acc) ->
    Acc.

match_head_list(X, Var, Acc) when is_atom(Var) ->
    case is_var(Var) of
	true ->
	    [{Var, X}|Acc];
	false ->
	    if X == Var ->
		    %% possible if both are non-proper lists
		    Acc;
	       true ->
		    throw(false)
	    end
    end;
match_head_list([H1|T1], [H2|T2], Acc) when is_atom(H2) ->
    case is_var(H2) of
	true -> match_head_list(T1,T2, [{H2,H1}|Acc]);
	false ->
	    if H1==H2 ->
		    match_head_list(T1,T2, Acc);
	       true ->
		    throw(false)
	    end
    end;
match_head_list([X|T1], [X|T2], Acc) ->
    match_head_list(T1,T2, Acc);
match_head_list([], [], Acc) ->
    Acc;
match_head_list(_, _, _) ->
    throw(false).

match_guards(Gs, Vars) ->    
    try lists:all(fun(G) ->
			  guard(G, Vars)
		  end, Gs)
    catch
	error:_ ->
	    false
    end.

prod([P], Obj, Vars) ->
    prod1(P, Obj, Vars).

prod1('$_', Obj, _Vars) ->
    Obj;
prod1('$$', _, Vars) ->
    %% Vars could contain duplicate assignments (the first instance in the
    %% list is the one that matters). 
    [Val || {_,Val} <- orddict:from_list(Vars)];
prod1(P, _, Vars) when is_atom(P) ->
    case lists:keysearch(P, 1, Vars) of
	false ->
	    case is_var(P) of
		true ->
		    erlang:error(badarg);
		false ->
		    P
	    end;
	{value,{_,Val}} ->
	    Val
    end;
prod1({const,V}, _Obj, _Vars) ->
    V;
prod1({Op,Expr}, Obj, Vars)
 when Op==float;
      Op==list_to_binary; Op==binary_to_list;
      Op==list_to_tuple; Op==tuple_to_list; 
      Op==integer_to_list; Op==list_to_integer;
      Op==atom_to_list; Op==list_to_atom;
      Op==trunc; Op==round; Op==abs ->
    erlang:Op(prod1(Expr,Obj,Vars));
prod1({Tuple}, Obj, Vars) when is_tuple(Tuple) ->
    if Tuple == {} ->
	    {};
       true ->
	    list_to_tuple(
	      lists:foldr(
		fun(P, Acc) ->
			[prod1(P, Obj, Vars)|Acc]
		end, [], tuple_to_list(Tuple)))
    end.

guard({Op, A, B}, Vars) when Op=='==';
			     Op=='=/=' ->
    Va = value_of(A, Vars),
    Vb = value_of(B, Vars),
    erlang:Op(Va, Vb);
guard({'let', Var, Expr, In}, Vars) ->
    case is_var(Var) andalso not(lists:keymember(Var, 1, Vars)) of
	true ->
	    Value = value_of(Expr, Vars),
	    guard(In, [{Var, Value}|Vars]);
	false ->
	    erlang:error(badarg)
    end;
guard({'andalso', A, B}, Vars) ->
    case guard(A, Vars) of
	true ->
	    guard(B, Vars);
	false ->
	    false
    end;
guard({'orelse', A, B}, Vars) ->
    case guard(A, Vars) of
	false ->
	    guard(B, Vars);
	true ->
	    true
    end;
guard({'hd', X}, Vars) ->
    hd(value_of(X, Vars));
guard({'tl', X}, Vars) ->
    tl(value_of(X, Vars)).

value_of(X, Vars) when is_atom(X) ->
    case is_var(X) of
	true ->
	    {value, {_,Val}} = lists:keysearch(X, 1, Vars),
	    Val;
	false ->
	    X
    end;
value_of({'hd', X}, Vars) ->
    hd(value_of(X, Vars));
value_of({'tl', X}, Vars) ->
    tl(value_of(X, Vars));
value_of({subterm, Expr, RecurseOp, While, Until}, Vars) ->
    Recurse = 
	case RecurseOp of
	    'tl' -> {'tl', '$_'};
	    {element, Pos} when is_integer(Pos), Pos > 0 -> 
		{element, Pos, '$_'};
	    _ ->
		erlang:error(badarg)
	end,
    CurVal = value_of(Expr, Vars),
    subterm(Recurse, While, Until, CurVal, Vars);
value_of(X, _Vars) ->
    X.


is_var('$_') -> true;
is_var(X) when is_atom(X) ->
    try begin 
	    "$" ++ T = atom_to_list(X),
	    N = list_to_integer(T),
	    (0 < N) and (N < 100000)
	end
    catch
	error:_ ->
	    false
    end;
is_var(_) ->
    false.

subterm(Recurse, While, Until, CurVal, Vars) ->
    Vars1 = [{'$_',CurVal}|Vars],
    case guard(Until, Vars1) of
	false ->
	    case guard(While, Vars1) of
		true ->
		    NewVal = value_of(Recurse, Vars1),
		    subterm(Recurse, While, Until, NewVal, Vars);
		false ->
		    CurVal
	    end;
	true ->
	    CurVal
    end.
		       
%%% ============================================


guards([eos]) ->
    fun(Vs) -> {'==', list_var(cur(Vs)), []} end;
guards([]) ->
    fun(_Vs) -> true end;
guards([E|Es]) ->
    Next = guards(Es),
    case E of
	I when is_integer(I) ->
	    fun(Vs) ->
		    Cur = cur(Vs),
		    V = char_var(Cur),
		    {'andalso', 
		     {'andalso', 
		      {'=/=', list_var(Cur), []},
		      {'==', V, I}}, next_guard(Next, Vs)}
	    end;
	{char_class, Chars} ->
	    fun(Vs) ->
		    Cur = cur(Vs),
		    Var = char_var(Cur),
		    Test = fun([C], _T1) ->
				   {'==', Var, C};
			      ([C|Cs], T1) ->
				   {'orelse', 
				    {'==', Var, C}, T1(Cs, T1)}
			   end,
		    {'andalso',
		     {'andalso',
		      {'=/=', list_var(Cur), []},
		      Test(Chars, Test)},
		     next_guard(Next, Vs)}
	    end;
	{comp_class, Chars} ->
	    fun(Vs) ->
		    Cur = cur(Vs),
		    Var = char_var(Cur),
		    Test = fun([C], _T1) ->
				   {'=/=', Var, C};
			      ([C|Cs], T1) ->
				   {'orelse', {'=/=', Var, C}, T1(Cs, T1)}
			   end,
		    {'andalso',
		     {'andalso',
		      {'=/=', list_var(Cur), []},
		      Test(Chars, Test)},
		     next_guard(Next, Vs)}
	    end;
	{kclosure, Closure} ->
	    kclosure(Closure, Next);
	{pclosure, Closure} ->
	    pclosure(Closure, Next)
    end.


