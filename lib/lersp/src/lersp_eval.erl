%%%-------------------------------------------------------------------
%%% File    : lersp_eval.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : interpeter/evaluator
%%%
%%% Created : 27 Nov 2001 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(lersp_eval).

-compile(export_all).

-import(lists, [reverse/1, foldl/3, foldl/3, flatten/1]).

-record(st,
	{
	  defines,			% dict() of (define ...)ed values
	  macros			% dict()
	 }).

-record(env, {parent, bindings}).
-record(bif, {name, type=medium, fn}).	% type = special | simple | medium
-record(proc, {args, body, env}).
-record(void, {}).

%% ----------------------------------------------------------------------
%% Frontend

eval_file(Fname, S0) ->
    case file:read_file(Fname) of
	{ok, Bin} ->
	    S = binary_to_list(Bin),
	    Xs = lersp_parse:parse_all(S),
	    eval_exprs(Xs, S0)
% 	Err ->
% 	    Err
    end.

eval_exprs(Xs) ->
    eval_exprs(Xs, state0()).

eval_exprs([], St0) ->
    {#void{}, St0};
eval_exprs([X], St0) ->
    eval_expr(X, St0);
eval_exprs([X|Xs], St0) ->
    {_Val, St1} = eval_expr(X, St0),
    eval_exprs(Xs, St1).

eval_expr(X0, St0) ->
    {X1, St1} = macro_expand(X0, St0),
    %%io:format("MACRO: ~p ~n         => ~p~n", [pprint(X0), pprint(X1)]),
    eval(X1, make_top_env(), St1).

state0() ->
    S0 = bind_bifs(#st{defines=dict:new(),
		       macros=dict:new()}),
    foldl(fun(F, S) ->
		  {_, Sn} = eval_file(filename:join(code:priv_dir(lersp),F),S),
		  Sn
	  end,
	  S0,
	  init_files()).

init_files() ->
    ["0-fundamental.ler",
     "core-macs.ler"].

bind_bifs(St0) ->
    foldl(fun({Name, Val}, St) ->
		  global_set(Name, Val, St)
	  end,
	  St0,
	  initial_bindings()).

%% ----------------------------------------------------------------------
%% Macro expansion

macro_expand([quote,X], St0) ->
    {[quote,X], St0};
macro_expand([quasiquote,X], St0) ->
    macro_expand_quasi([quasiquote,X], 0, St0);
macro_expand([S|Args], St0) when atom(S) ->
    case macro_lookup(S, St0) of
	{value, Proc} ->
	    {Res, St1} = apply_proc(Proc, Args, St0),
	    macro_expand(Res, St1);
	unbound ->
	    macro_expand_list([S|Args], St0)
    end;
macro_expand([X|Xs], St0) ->
    macro_expand_list([X|Xs], St0);
macro_expand(X, St0) ->
    {X, St0}.

macro_expand_list(Xs, St) -> macro_expand_list(Xs, St, []).

macro_expand_list([X|Xs], St0, Acc) ->
    {Res, St1} = macro_expand(X, St0),
    macro_expand_list(Xs, St1, [Res|Acc]);
macro_expand_list([], St, Acc) ->
    {reverse(Acc), St};
macro_expand_list(X, St0, Acc) ->	% for improper lists
    {Res, St1} = macro_expand(X, St0),
    {reverse(Acc) ++ Res, St1}.

macro_expand_quasi([quasiquote,X], D, St0) ->
    {Res, St1} = macro_expand_quasi(X, D+1, St0),
    {[quasiquote,Res], St1};
macro_expand_quasi([UQ,X], D, St0)
  when UQ == unquote; UQ == 'unquote-splicing' ->
    if
	D == 1 ->
	    {Res, St1} = macro_expand(X, St0),
	    {[UQ,Res], St1};
	true ->
	    {Res, St1} = macro_expand_quasi(X, D-1, St0),
	    {[UQ,Res], St1}
    end;
macro_expand_quasi(L, D, St0) when list(L) ->
    macro_expand_quasi_list(L, D, St0, []);
macro_expand_quasi(X, D, St0) ->
    {X, St0}.

macro_expand_quasi_list([X|Xs], D, St0, Acc) ->
    {Res, St1} = macro_expand_quasi(X, D, St0),
    macro_expand_quasi_list(Xs, D, St1, [Res|Acc]);
macro_expand_quasi_list([], D, St0, Acc) ->
    {reverse(Acc), St0};
macro_expand_quasi_list(X, D, St0, Acc) -> % improper
    {reverse(Acc) ++ D, St0}.

%% ----------------------------------------------------------------------
%% Evaluator

eval(X, E, St0) ->
    case type(X) of
	integer ->
	    {X,St0};
	symbol ->
	    case lookup_var(X, E, St0) of
		{value, V} ->
		    {V, St0};
		unbound ->
		    exit({unbound, X})
	    end;
	void ->
	    {X, St0};
	null ->
	    {X, St0};
	list ->
	    [Rator|Rands] = X,
	    apply_it(Rator, Rands, E, St0)
    end.

apply_it(Rator, Rands, E, St0) ->
    {Proc, St1} = eval(Rator, E, St0),
    case type(Proc) of
	proc ->
	    {Args, St2} = eval_list(Rands, E, St1),
	    apply_proc(Proc, Args, St2);
	bif ->
	    {Args, St2} = case Proc#bif.type of
			      special ->
				  {Rands, St1};
			      _ ->
				  eval_list(Rands, E, St1)
			  end,
	    apply_bif(Proc, Args, E, St2);
	Other ->
	    error("bad procedure: ~p", [Proc])
    end.

apply_bif(Bif, Args, E, St) when Bif#bif.type == simple ->
    {apply(Bif#bif.fn, [Args]), St};
apply_bif(Bif, Args, E, St) ->
    apply(Bif#bif.fn, [Args,E,St]).

apply_proc(#proc{args=ArgNames, body=Body, env=ProcEnv}, ArgVals, St) ->
    EvalEnv = env_extend(ProcEnv, ArgNames, ArgVals),
    eval_seq(Body, EvalEnv, St).

eval_list(Xs, E, S) ->
    eval_list(Xs, E, S, []).

eval_list([], E, S, Acc) ->
    {reverse(Acc), S};
eval_list([X|Xs], E, S0, Acc) ->
    {Val, S1} = eval(X, E, S0),
    eval_list(Xs, E, S1, [Val|Acc]).

eval_seq([], E, St0) ->
    {#void{}, St0};
eval_seq([X], E, St0) ->
    eval(X, E, St0);
eval_seq([X|Xs], E, St0) ->
    {_Val,St1} = eval(X, E, St0),
    eval_seq(Xs, E, St1).

%% ----------------------------------------------------------------------
%% Environments and global variables

%% Returns: {Env, St1}
make_top_env() ->
  env_extend(undefined, [], []).

env_extend(ParentEnv, Names, Values) ->
    Bindings = foldl(fun({N,V}, Dict) -> dict:store(N, V, Dict) end,
		     dict:new(),
		     join_lambdalist(Names, Values)),
    #env{parent=ParentEnv, bindings=Bindings}.

join_lambdalist([], [])            -> [];
join_lambdalist(X, Y) when atom(X) -> [{X, Y}];
join_lambdalist([X|Xs], [Y|Ys])    -> [{X,Y}|join_lambdalist(Xs,Ys)].

%% Returns: {value, V} | unbound
lookup_var(Key, Env, St) ->
    case dict:find(Key, Env#env.bindings) of
	{ok, Value} ->
	    {value, Value};
	error ->
	    case Env#env.parent of
		undefined ->
		    case dict:find(Key, St#st.defines) of
			{ok, V} ->
			    {value, V};
			error ->
			    unbound
		    end;
		P ->
		    lookup_var(Key, P, St)
	    end
    end.

%% Returns: St'
global_set(Name, Value, St = #st{defines=D}) ->
    St#st{defines=dict:store(Name, Value, D)}.

macro_set(Name, Value, St = #st{macros=D}) ->
    St#st{macros=dict:store(Name, Value, D)}.

macro_lookup(Name, #st{macros=D}) ->
    case dict:find(Name, D) of
	{ok, V} -> {value, V};
	error   -> unbound
    end.


%% ----------------------------------------------------------------------
%% BIFs

initial_bindings() ->
    [{BIF#bif.name, BIF} || BIF <- specials()] ++
	[{Name, #bif{name=Name, type=simple, fn={lersp_bif, Name}}} ||
	    Name <- lersp_bif:names()].

specials() ->
    [
     #bif{name='define', type=special, fn=fun bif_define/3},
     #bif{name='defmacro', type=special, fn=fun bif_defmacro/3},
     #bif{name='lambda', type=special, fn=fun bif_lambda/3},
     #bif{name='quote', type=special, fn=fun bif_quote/3},
     #bif{name='quasiquote', type=special, fn=fun bif_quasiquote/3},
     #bif{name='cond', type=special, fn=fun bif_cond/3},
     #bif{name='apply', type=medium, fn=fun bif_apply/3}
    ].

bif_define([[Name|Args]|Body], E, St0) ->
    bif_define([Name,[lambda,Args|Body]], E, St0);
bif_define([Sym, X], E, St0) when atom(Sym) ->
    {Val, St1} = eval(X, E, St0),
    {true, global_set(Sym, Val, St0)}.

bif_defmacro([Sym, X], E, St0) ->
    {Proc, St1} = eval(X, E, St0),
    proc = type(Proc),			% type check
    {true, macro_set(Sym, Proc, St0)}.

bif_lambda([Args|Body], E, St0) ->
    {#proc{args=Args, body=Body, env=E}, St0}.

bif_quote([X], E, St0) ->
    {X, St0}.

bif_quasiquote([X], E, St0) ->
    quasi(X, E, St0, 0).

%% Hopefully this is what they meant in R5RS...
quasi([unquote, X], E, St0, 0) ->
    eval(X, E, St0);
quasi([quasiquote, X], E, St0, D) ->
    quasi(X, E, St0, D+1);
quasi([unquote, X], E, St0, D) ->
    quasi(X, E, St0, D-1);
quasi(Xs, E, St0, D) when list(Xs) ->
    quasi_list(Xs, E, St0, D, []);
quasi(X, E, St0, D) ->
    {X, St0}.

quasi_list([['unquote-splicing', X]|Xs], E, St0, 0, Acc) ->
    {V, St1} = eval(X, E, St0),
    quasi_list(Xs, E, St1, 0, reverse(V) ++ Acc);
quasi_list([X|Xs], E, St0, D, Acc) ->
    {V,St1} = quasi(X, E, St0, D),
    quasi_list(Xs, E, St1, D, [V|Acc]);
quasi_list([], E, St, D, Acc) ->
    {reverse(Acc), St}.

bif_cond([], E, St0) ->
    {#void{}, St0};
bif_cond([[else,Then]|Xs], E, St0) ->
    eval(Then, E, St0);
bif_cond([[If,Then]|Xs], E, St0) ->
    case eval(If, E, St0) of
	{false, St1} ->
	    bif_cond(Xs, E, St1);
	{Val, St1} ->
	    case Then of
		['=>', Receiver] ->
		    {Proc, St2} = eval(Receiver, E, St1),
		    eval([Proc, Val], E, St2);
		_ ->
		    eval(Then, E, St1)
	    end
    end.

bif_apply([P, Args], E, St0) ->
    case type(P) of
	proc ->
	    apply_proc(P, Args, St0);
	bif when P#bif.type /= special ->
	    apply_bif(P, Args, E, St0)
    end.

%% ----------------------------------------------------------------------

type([])                     -> null;
type(X) when integer(X)      -> integer;
type(X) when atom(X)         -> symbol;
type(X) when list(X)         -> list;
type(X) when record(X, bif)  -> bif;
type(X) when record(X, proc) -> proc;
type(X) when record(X, void) -> void.

%% ----------------------------------------------------------------------
%% Pretty printing

pprint(X) ->
    flatten(case type(X) of
		integer -> integer_to_list(X);
		symbol  -> atom_to_list(X);
		null    -> "()";
		list    -> pp_list(X);
		bif     -> io_lib:format("#<bif ~s>", [X#bif.name]);
		proc    -> io_lib:format("#<proc ~s>", [pprint(X#proc.args)]);
		void    -> "#<void>"
	    end).

pp_list(L)  -> ["(",pp_list1(L),")"].

pp_list1([X])                -> pprint(X);
pp_list1([H|T]) when list(T) -> [pprint(H), " ", pp_list1(T)];
pp_list1([H|T])              -> [pprint(H), " . ", pprint(T)].


%% ----------------------------------------------------------------------
%% Error handling

error(Reason) ->
    exit({error, Reason}).

error(Reason, FmtArgs) ->
    exit({error, flatten(io_lib:format(Reason, FmtArgs))}).

