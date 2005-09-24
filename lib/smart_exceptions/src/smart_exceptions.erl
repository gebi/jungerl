%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (C) 2003 Thomas Lindgren <thomasl_erlang@yahoo.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%%			   SMART EXCEPTIONS
%%
%% Author: Thomas Lindgren (030414-)
%%
%% A simplified version of the earlier smart_exceptions.
%%
%% USAGE
%%   erlc +'{parse_transform, smart_exceptions}' file.erl
%%
%% As given, the code generates a "smart exit", an exit with more info
%% than is usual. Uncomment the -define(exn_handler,...) to instead
%% invoke ?default_exn_handler_mod:* when there is an exit.
%%
%% PURPOSE:
%%
%% Rather than generating a terse exception 'badarg', this preprocessor
%% rewrites exceptions (apart from 'function undefined') to do one of:
%%  - invoke an exception handler, smart_exc_rt.erl (or user defined)
%%    * includes giving some BIF context
%%  - generate a 'big exit' with module, function, line, reason
%%
%% The generated code looks awful (lots of redundant code) but the beam
%% compiler gets rid of this.
%%
%% NOTE: file/2 and file/3 can NOT be used without the parse.erl module. For
%% external distribution, just use parse_transform/2
%%
%% *** UNFINISHED ***
%% - function undefined exceptions not caught, likewise for funs
%%   * see error_handler:undefined_function/3 undefined_lambda/3
%%     however, we can't redefine this globally
%%     [the solution would be for the new module to invoke error_handler
%%      and catch any exits; however, it also has to retain line numbers
%%      etc somehow]
%% - smart_exc_rt functionality not tested lately
%%   * it used to work :-)

-module(smart_exceptions).
-author('thomasl_erlang@yahoo.com').
-export([parse_transform/2]).
-export([file/2, file/3]).    %% only for internal use

-define(default_exn_handler_mod, smart_exc_rt).

-define(exn_handler, smart_exit).
%%-define(exn_handler, ?default_exn_handler_mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_transform(Forms, Opts) ->
    %% Opts = compiler options
    M = get_module_name(Forms),
    Handler = get_exc_handler(),
    case Handler of
	none ->
	    Forms;
	_ ->
	    forms(M, Handler, Forms)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_exc_handler() ->
    case init:get_argument(smart_exceptions) of
	{ok, Vals} ->
	    [Handler_str] = lists:last(Vals),
	    case list_to_atom(Handler_str) of
		exit ->
		    smart_exit;
		none ->
		    %% don't transform
		    none;
		Hdlr ->
		    Hdlr
	    end;
	Err ->
	    ?exn_handler
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file(File, Opts) ->
    Mod = parse:file(File, Opts),
    NewMod = module(Mod),
    parse:print(parse:reattribute(NewMod)).

file(File, Outfile, Opts) ->
    Mod = parse:file(File, Opts),
    NewMod = module(Mod),
    parse:print(Outfile, parse:reattribute(NewMod)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module({Mod, Exp, Forms, Misc}) ->    
    Handler = get_exc_handler(),
    {Mod, Exp, forms(Mod, Handler, Forms), Misc}.

forms(M, Handler, Forms) when atom(M), atom(Handler) ->
    [ form(M, Handler, Form) || Form <- Forms ].

-ifdef(om).
form(M, Handler, Form) ->
    {F, A} = function_name(Form),
    mapform:form(
      fun({function, Lf, F1, A1, Clss}) ->
	      {function, Lf, F1, A1, 
	       handle_function_clause(M, F, A, Lf, Clss, Handler)};
	 (Attr) ->
	      Attr
      end,
      fun id/1,
      fun id/1,
      fun id/1,
      fun({match, Lm, P, E}=Expr) ->
	      handle_match(M, F, A, Lm, P, E, Handler);
	 ({'case',Lc,E,Clss}) ->
	      {'case',Lc, E, handle_case_clause(M, F, A, Lc, Clss, Handler)};
	 ({'if',Li,Clss}) ->
	      {'if', Li, handle_if_clause(M, F, A, Li, Clss, Handler)};
	 ({'fun',Lf,{clauses,Clss}}) ->
	      {'fun',Lf,
	       {clauses, handle_function_clause(M, F, A, Lf, Clss, Handler)}};
	 ({'fun',Lf,{clauses,Clss}, Info}) ->
	      {'fun',Lf,{clauses, 
			 handle_function_clause(M, F, A, Lf, Clss, Handler)},
	       Info};
	 ({op,Lo,Op,E1,E2}=E) ->
	      handle_binop(M, F, A, Lo, Op, E1, E2, Handler);
	 ({op,Lo,Op,E1}=E) ->
	      handle_unop(M, F, A, Lo, Op, E1, Handler);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,exit}},[Rsn]}=E) ->
	      handle_exit(M, F, A, Lc, Rsn, Handler);
	 ({call,Lc,{atom,Lf,exit},[Rsn]}=E) ->
	      handle_exit(M, F, A, Lc, Rsn, Handler);
	 ({call,Lc,{remote,Lr,{atom,Lm,Mod},{atom,Lf,Fn}},As}=E) ->
	      case erlang:is_builtin(Mod, Fn, length(As)) of
		  true ->
		      handle_bif(M, F, A, Lc, 
				 Mod, Fn, length(As), As, Handler);
		  false ->
		      E
	      end;
	 (E) ->
	      E
      end,
      Form
     ).

id(X) ->
    X.
-else.
form(M, Handler, Form) ->
    {F, A} = function_name(Form),
    mapform0(
      fun({function, Lf, F1, A1, []}=TheForm) ->
	      TheForm;
	 ({function, Lf, F1, A1, Clss}) ->
	      {function, Lf, F1, A1, 
	       handle_function_clause(M, F, A, Lf, Clss, Handler)};
	 ({match, Lm, P, E}=Expr) ->
	      handle_match(M, F, A, Lm, P, E, Handler);
	 ({'case',Lc,E,Clss}) ->
	      {'case',Lc, E, handle_case_clause(M, F, A, Lc, Clss, Handler)};
	 ({'if',Li,Clss}) ->
	      {'if', Li, handle_if_clause(M, F, A, Li, Clss, Handler)};
	 ({'fun',Lf,{clauses,Clss}}) ->
	      {'fun',Lf,
	       {clauses, handle_function_clause(M, F, A, Lf, Clss, Handler)}};
	 ({'fun',Lf,{clauses,Clss}, Info}) ->
	      {'fun',Lf,{clauses, 
			 handle_function_clause(M, F, A, Lf, Clss, Handler)},
	       Info};
	 ({op,Lo,Op,E1,E2}=E) ->
	      handle_binop(M, F, A, Lo, Op, E1, E2, Handler);
	 ({op,Lo,Op,E1}=E) ->
	      handle_unop(M, F, A, Lo, Op, E1, Handler);
	 ({call,Lc,{remote,Lr,{atom,Lm,erlang},{atom,Lf,exit}},[Rsn]}=E) ->
	      handle_exit(M, F, A, Lc, Rsn, Handler);
	 ({call,Lc,{atom,Lf,exit},[Rsn]}=E) ->
	      handle_exit(M, F, A, Lc, Rsn, Handler);
	 ({call,Lc,{remote,Lr,{atom,Lm,Mod},{atom,Lf,Fn}},As}=E) ->
	      case erlang:is_builtin(Mod, Fn, length(As)) of
		  true ->
		      handle_bif(M, F, A, Lc, 
				 Mod, Fn, length(As), As, Handler);
		  false ->
		      E
	      end;
	 (E) ->
	      E
      end,
      Form
     ).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% To suppress warnings for clauses that cannot match in R10B,
%% we mark the clauses as compiler-generated by giving it a negative
%% line number. That is probably harmless in pre-R10B versions.
%% /Bjorn Gustavsson
%%


handle_function_clause(M, F, A, Lf, Clss, Handler) ->
    N = clauses_arity(Clss),
    Exc_hd = mk_vars(1,N),
    Clss ++ 
	[{clause, -Lf, Exc_hd, [], 
	  [clause_handler(Handler, M, F, A, Lf, function_clause, Exc_hd)]}].

handle_case_clause(M, F, A, Lf, Clss, Handler) ->
    N = clauses_arity(Clss),
    Exc_hd = mk_vars(1,N),
    Clss ++ 
	[{clause, -Lf, Exc_hd, [], 
	  [clause_handler(Handler, M, F, A, Lf, case_clause, Exc_hd)]}].

handle_if_clause(M, F, A, Lf, Clss, Handler) ->
    N = clauses_arity(Clss),
    Exc_hd = mk_vars(1,N),
    Clss ++ 
	[{clause, -Lf, Exc_hd, [], 
	  [clause_handler(Handler, M, F, A, Lf, if_clause, Exc_hd)]}].

%% Note: we match the handler "P = <handler>" since
%% erlc will think the clause is unsafe otherwise (since it always exits
%% it's actually safe).
%%
%% case E of
%%   X=P -> X;
%%   X -> <match handler>
%% end
%%
%% where X is a new variable

handle_match(M, F, A, Lm, P, E, Handler) ->
    X = new_var(),
    Exc_hd = [X],
    {'case', Lm, E,
     [{clause, Lm, [{match, Lm, X, P}], [], [X]},
      {clause, -Lm, Exc_hd, [],
       [{match, Lm, P, 
	 clause_handler(Handler, M, F, A, Lm, match, Exc_hd)}]}]}.

handle_binop(M, F, A, Lo, Op, E1, E2, Handler) ->
    Rsn = new_var(),
    X1 = new_var(),
    X2 = new_var(),
    Res = new_var(),
    {block, Lo,
     [{match, Lo, X1, E1},
      {match, Lo, X2, E2},
      {'case', Lo, {'catch', Lo, {op, Lo, Op, X1, X2}},
       [{clause, Lo, [{tuple, Lo, [{atom, Lo, 'EXIT'}, Rsn]}], [],
	 [op_handler(Handler, M, F, A, Lo, Rsn, binop, Op, [X1,X2])]},
	{clause, Lo, [Res], [], [Res]}]}
     ]
    }.

handle_unop(M, F, A, Lo, Op, E1, Handler) ->
    Rsn = new_var(),
    X1 = new_var(),
    Res = new_var(),
    {block, Lo,
     [{match, Lo, X1, E1},
      {'case', Lo, {'catch', Lo, {op, Lo, Op, X1}},
       [{clause, Lo, [{tuple, Lo, [{atom, Lo, 'EXIT'}, Rsn]}], [],
	 [op_handler(Handler, M, F, A, Lo, Rsn, unop, Op, [X1])]},
	{clause, Lo, [Res], [], [Res]}]}
     ]
    }.

handle_exit(M, F, A, Le, Rsn, Handler) ->
    exit_handler(Handler, M, F, A, Le, Rsn).

handle_bif(M, F, A, Lb, Mod, Fn, Ar, Args, Handler) ->
    Xs = mk_vars(1, Ar),
    BIF_call = {call, Lb,
		{remote, Lb, {atom, Lb, Mod}, {atom, Lb, Fn}},
		Xs},
    Rsn = new_var(),
    Res = new_var(),
    {block, Lb,
     [ {match, Lb, X, Arg} || {X, Arg} <- zip(Xs, Args) ]
     ++ [{'case', Lb, {'catch', 0, BIF_call},
	  [{clause, Lb, [{tuple, Lb, [{atom, Lb, 'EXIT'}, Rsn]}], [],
	    [op_handler(Handler, M, F, A, Lb, Rsn, bif, {Mod, Fn}, Xs)]},
	   {clause, Lb, [Res], [], [Res]}]}]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% There are two versions of these.
%% - first version: just exit with lots more information
%% - second version: call smart_exc:handler(...)

%% For exit
%%
%% Note that Rsn is a syntax tree while the others are terms

exit_handler(Handler, M, F, A, Line, Rsn) ->
    case ?exn_handler of
	smart_exit ->
	    Args = 
		[ erl_parse:abstract(Term) 
		  || Term <- [{M, F, A}, {line, Line}] ] ++ [Rsn],
	    mk_exit(Line, Args);
	smart_exc ->
	    Args = 
		[ erl_parse:abstract(Term) 
		  || Term <- [M, F, A, Line] ] ++ [Rsn],
	    mk_invoke(Line, Handler, Args)
    end.

%% For clauses
%%
%% Rsn is an atom/term, while Exc_hd is a list of syntax
%% trees (vars)

clause_handler(Handler, M, F, A, Line, Rsn, Exc_hd) ->
    case ?exn_handler of
	smart_exit ->
	    Args = [ erl_parse:abstract(Term) 
		     || Term <- [{M, F, A}, {line, Line}, Rsn] ]
		++ [cons_list(Exc_hd)],
	    mk_exit(Line, Args);
	smart_exc ->
	    Args = [ erl_parse:abstract(Term) 
		     || Term <- [M, F, A, Line, Rsn] ] 
		++ [cons_list(Exc_hd)],
	    mk_invoke(Line, Handler, Rsn, Args)
    end.

%% For op/bif
%%
%% Note that Op term, Rsn syntax tree, while Exc_hd is a list of syntax
%% trees (vars)
%%
%% Note: we should perhaps format the exception a wee bit differently.
%% Currently: {..., {M,F}, As} should be {...,{M,F,As}}?

op_handler(Handler, M, F, A, Line, Rsn, Ty, Op, Exc_hd) ->
    case ?exn_handler of
	smart_exit ->
	    Args = [ erl_parse:abstract(Term) 
		     || Term <- [{M, F, A}, {line, Line}] ]
		++ [Rsn, erl_parse:abstract(Op), cons_list(Exc_hd)],
	    mk_exit(Line, Args);
	smart_exc ->
	    Args = [ erl_parse:abstract(Term) 
		     || Term <- [{M, F, A}, {line, Line}] ] 
		++ [Rsn, erl_parse:abstract(Op), cons_list(Exc_hd)],
	    mk_invoke(Line, Handler, Ty, Args)
    end.

mk_exit(Line, Args) ->
    {call, Line, 
     {atom, Line, exit}, 
     [{tuple, Line, Args}]}.

mk_invoke(Line, Handler, Args) ->
    mk_invoke(Line, Handler, exit, Args).

mk_invoke(Line, Handler, Rsn, Args) ->
    {call, Line, 
     {remote, Line, {atom, Line, Handler}, {atom, Line, Rsn}}, 
     Args}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clauses_arity([{clause, _, H, G, B}|_]) ->
    length(H).

%%

mk_vars(M,N) ->
    if
	M > N ->
	    [];
	true ->
	    [new_var()|mk_vars(M+1,N)]
    end.

%%

new_var() ->
    K = counter('exc var counter'),
    {var,0,list_to_atom("_" ++ integer_to_list(K))}.

%% from ap_util.erl

counter(Name) ->
    Ix =
	case get(Name) of
	    N when integer(N) ->
		N;
	    undefined ->
		0
	end,
    put(Name,Ix+1),
    Ix.

%%

get_module_name([{attribute,Lm,module,M}|Xs]) ->
    M;
get_module_name([_|Xs]) ->
    get_module_name(Xs).

%%

function_name({function, Lf, F, A, Clss}) ->
    {F, A};
function_name(Other) ->
    {not_a_function, no_name}.

%%

zip([X|Xs], [Y|Ys]) ->
    [{X,Y} | zip(Xs, Ys)];
zip([], []) ->
    [].

%%

cons_list([X|Xs]) ->
    {cons, 0, X, cons_list(Xs)};
cons_list([]) ->
    {nil, 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Simple version of mapform.erl:

mapform0(F, {clause, Lc, H, G, B}) ->
    F({clause, Lc, H, G, mapform0(F, B)});
mapform0(F, {match, Lc, P, E}) ->
    F({match, Lc, P, mapform0(F, E)});
mapform0(F, {lc, Llc, E, GQs}) ->
    F({lc, Llc, mapform0(F, E), [ mapform1(F, GQ) || GQ <- GQs ]});
mapform0(F, T) when tuple(T) ->
    F(list_to_tuple([ mapform0(F, Tsub) || Tsub <- tuple_to_list(T) ]));
mapform0(F, Xs) when list(Xs) ->
    [ mapform0(F, X) || X <- Xs ];
mapform0(F, C) when constant(C) ->
    C.

%% detect + elide pattern in qualifier

mapform1(F, {generate, Lg, P, E}) ->
    {generate, Lg, P, mapform0(F, E)};
mapform1(F, Qual) ->
    mapform0(F, Qual).
