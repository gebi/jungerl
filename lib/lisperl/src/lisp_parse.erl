%%%-------------------------------------------------------------------
%%% File    : lisp_parse.erl
%%% Author  : Vlad Dumitrescu <vlad_dumitrescu@hotmail.com>
%%% Description : A generic (mostly) CommonLisp compliant parser for lisp-like syntax.
%%%               Char syntax types and behaviour are defined in a separate module. 
%%%
%%% Created : 30 Mar 2004 by Vlad Dumitrescu <vlad_dumitrescu@hotmail.com>
%%%
%%% To do:
%%%    - error handling (recovery, continue)
%%%    - mktoken is now using Erlang synatx
%%%    - line numbers for macro structures: right now the last line is returned, 
%%%      not the first. General handling, or in each macro reader?
%%%    - use other sources than strings: binaries, files. 
%%%
%%% $Id$
%%%
%%%-------------------------------------------------------------------
-module(lisp_parse).

-export([new_env/0,
	 get_readtable_case/1,
	 set_readtable_case/2,
	 def_char/2,
	 def_chars/2,
	 get_syntax_type/2,
	 is_macro_char/2,
	 is_terminating_macro_char/2,
	 is_dispatching_macro_char/2,
	 get_macro_reader/2,
	 get_dispatch_reader/2,
	 def_macro_char/3,
	 def_macro_chars/2,
	 def_dispatch_char/3,
	 def_dispatch_chars/2,

	 parse/3,
	 parse_all/3,
	 parse_file/2,

	 parse_token/4,

	 strip_lines/1
	]).
%-compile(export_all).

%%%% Syntax 

-record(char_data, {type = illegal, 
		    %% [illegal, whitespace, macro, 
		    %%  constituent, single_escape, multiple_escape]
		    terminating = false, %% macro
		    dispatching = false, %% macro
		    macro = fun null_macro/4,
		    dispatch = fun null_dispatch/5,
		    attribute = [illegal]
		    %% [illegal, alphabetic, alphadigit, package_marker,
		    %% sign, ratio_marker, dot, decimal_point, 
		    %% single_float_marker, double_float_marker,
		    %% short_float_marker, long_float_marker]
		   }).

-record(lisp_env, {readtable = erlang:make_tuple(128,#char_data{}),
		   readtable_case = keep %% upper | lower | keep
		  }).

-define(DBG(X), erlang:display({dbg, X})).

new_env() ->
    #lisp_env{}.

get_readtable_case(Env) ->
    Env#lisp_env.readtable_case.

set_readtable_case(Env, Val) ->
    Env#lisp_env{readtable_case = Val}.

get_char_data(C, Env) when C>=0, C=<127->
    #lisp_env{readtable=R} = Env,
    element(C+1, R);
get_char_data(C, Env) ->
    get_char_data($A, Env).

set_char_data(C, Env, Data) ->
    #lisp_env{readtable=R} = Env,
    Env#lisp_env{readtable=setelement(C+1, R, Data)}.

def_char({C, macro, terminating, dispatching}, Env) ->
    set_syntax_type(C, {macro, true, true}, Env);
def_char({C, macro, terminating}, Env) ->
    set_syntax_type(C, {macro, true}, Env);
def_char({C, macro, non_terminating}, Env) ->
    set_syntax_type(C, {macro, false, false}, Env);
def_char({C, T}, Env) ->
    set_syntax_type(C, T, Env).

def_chars(L, Env) ->
    lists:foldl(fun(X, Env_)->
			  def_char(X, Env_)
		  end, Env, L).

get_syntax_type(C, Env) ->
    Data = get_char_data(C, Env),
    Data#char_data.type.

set_syntax_type(C, {T, Term, Disp}, Env) ->
    Data = get_char_data(C, Env),
    Data1 = Data#char_data{type=T, terminating=Term, dispatching=Disp},
    set_char_data(C, Env, Data1);
set_syntax_type(C, {T, Term}, Env) ->
    Data = get_char_data(C, Env),
    Data1 = Data#char_data{type=T, terminating=Term, dispatching=false},
    set_char_data(C, Env, Data1);
set_syntax_type(C, T, Env) ->
    Data = get_char_data(C, Env),
    Data1 = Data#char_data{type=T, terminating=false, dispatching=false},
    set_char_data(C, Env, Data1).

is_macro_char(C, Env) ->
    Data = get_char_data(C, Env),
    Data#char_data.type == macro.

is_terminating_macro_char(C, Env) ->
    Data = get_char_data(C, Env),
    (Data#char_data.type == macro) and (Data#char_data.terminating==true).

is_dispatching_macro_char(C, Env) ->
    Data = get_char_data(C, Env),
    (Data#char_data.type == macro) and (Data#char_data.dispatching==true).

def_macro_char(C, Fun, Env) ->
    Data = get_char_data(C, Env),
    Data1 = Data#char_data{type=macro, macro=Fun},
    set_char_data(C, Env, Data1).

def_dispatch_char(C, Fun, Env) ->
    Data = get_char_data(C, Env),
    Data1 = Data#char_data{dispatch=Fun},
    set_char_data(C, Env, Data1).

get_macro_reader(C, Env) ->
    Data = get_char_data(C, Env),
    Data#char_data.macro.

get_dispatch_reader(C, Env) ->
    Data = get_char_data(C, Env),
    Data#char_data.dispatch.

def_macro_chars(L, Env) ->
    Fun = fun({C, F}, Acc) ->
		  def_macro_char(C, F, Acc)
	  end,
    lists:foldl(Fun, Env, L).

def_dispatch_chars(L, Env) ->
    Fun = fun({C, F}, Acc) ->
		  def_dispatch_char(C, F, Acc)
	  end,
    lists:foldl(Fun, Env, L).

%%% Parsing

parse_all(S, N, Env) ->
    case catch parse(S, N, Env) of
	nothing ->
	    [];
 	{nothing, Rest} ->
 	    parse_all(Rest, N, Env);
	{sexp, {Sexp, NN}, Rest} ->
	    [{Sexp, NN} | parse_all(Rest, NN, Env)];
	{error, Reason, NN, Rest} ->
	    [{error, Reason, NN} | parse_all(Rest, NN, Env)]
    end.

parse_file(File, Env) ->
    {ok, Bin} = file:read_file(File),
    S = fix_endlines(binary_to_list(Bin)),
    parse_all(S, 1, Env).

parse([], N, Env) ->
    nothing;
parse([C|T], N, Env) ->
    Type = get_syntax_type(C, Env),
    case Type of
	illegal ->
	    throw({error, {illegal_char, C}, N, T});
	whitespace ->
	    N1 = if C == $\n -> N + 1;
		    true -> N
		 end,
	    parse(T, N1, Env);
	macro->
	    case is_dispatching_macro_char(C, Env) of
		true ->
		    parse_dispatch(T, C, N, Env);
		false ->
		    F = get_macro_reader(C, Env),
		    case F of 
			undefined ->
			    throw({error, {undefined_macro, C}, N, T});
			F -> %when is_function(F) ->
			    case F(T, C, N, Env) of
				{nothing, Rest} ->
				    parse(T, N, Env);
				Result ->
				    Result
			    end
		    end
	    end;
	single_escape ->
	    case T of
		[] ->
		    throw({error, unexpected_eof, N, []});
		[Ch|T1] ->
		    parse_token(T1, N, Env, [Ch])
	    end;
	multiple_escape ->
	    parse_escaped_token(T, N, Env, []);
	constituent ->
	    parse_token(T, N, Env, [fix_char(C, Env)])
    end.

parse_token([], N, Env, Acc) ->
    {sexp, {mktoken(lists:reverse(Acc)), N}, []};
parse_token([C|T], N, Env, Acc) ->
    Type = get_syntax_type(C, Env),
    case Type of
	illegal ->
	    throw({error, {illegal_char, C}, N, T});
	whitespace ->
	    {sexp, {mktoken(lists:reverse(Acc)), N}, [C|T]};
	macro ->
	    case is_terminating_macro_char(C, Env) of
		true ->
		    {sexp, {mktoken(lists:reverse(Acc)), N}, [C|T]};
		false ->
		    parse_token(T, N, Env, [C|Acc])
	    end;
	single_escape ->
	    case T of
		[] ->
		    throw({error, unexpected_eof, N, []});
		[Ch|T1] ->
		    N1 = if Ch == $\n -> N + 1;
			    true -> N
			 end,
		    parse_token(T1, N1, Env, [Ch|Acc])
	    end;
	multiple_escape ->
	    parse_escaped_token(T, N, Env, Acc);
	constituent ->
	    parse_token(T, N, Env, [fix_char(C, Env)|Acc])
    end.

parse_escaped_token([], N, Env, Acc) ->
    throw({error, unexpected_eof, N, []});
parse_escaped_token([C|T], N, Env, Acc) ->
    Type = get_syntax_type(C, Env),
    case Type of
	illegal ->
	    throw({error, {illegal_char, C}, N, T});
	whitespace ->
	    N1 = if C == $\n -> N + 1;
		    true -> N
		 end,
	    parse_escaped_token(T, N1, Env, [C|Acc]);
	macro ->
	    parse_escaped_token(T, N, Env, [C|Acc]);
	single_escape ->
	    case T of
		[] ->
		    throw({error, unexpected_eof, N, []});
		[Ch|T1] ->
		    N1 = if Ch == $\n -> N + 1;
			    true -> N
			 end,
		    parse_escaped_token(T1, N1, Env, [Ch|Acc])
	    end;
	multiple_escape ->
	    parse_token(T, N, Env, Acc);
	constituent ->
	    parse_escaped_token(T, N, Env, [C|Acc])
    end.

mktoken(L) ->
    case catch list_to_float(L) of
	{'EXIT', Reason} ->
	    case catch list_to_integer(L) of
		{'EXIT', Reason2} ->
		    list_to_atom(L);
		    %{atom, L};
		N ->
		    N
	    end;
	F ->
	    F
    end.

parse_dispatch(L, C, N, Env) ->
    case parse_dispatch_arg(L, N, Env) of
	nothing ->
	    throw({error, unexpected_eof, N, L});
	{F, Arg, Rest} ->
	    Fun = get_dispatch_reader(F, Env),
	    Fun(Rest, Arg, F, N, Env)
    end.

parse_dispatch_arg([C|L], N, Env) ->		   
    {C, 0, L}.

strip_lines([H|T]) ->
    [strip_lines(H) | strip_lines(T)];
strip_lines({Sexp, N}) when is_integer(N) ->
    strip_lines(Sexp);
strip_lines(L) when is_tuple(L) ->
    list_to_tuple([strip_lines(X) || X <- tuple_to_list(L)]);
strip_lines(X) ->
    X.

null_macro(L, C, N, Env) -> 
    {sexp, {[C], N}, L}.

null_dispatch(L, C, Arg, N, Env) -> 
    {nothing, L}.


fix_endlines(L) ->
    fix_endlines(L, []).

fix_endlines([], R) ->
    lists:reverse(R);
fix_endlines([$\r,$\n|T], R) ->
    fix_endlines(T, [$\n|R]);
fix_endlines([H|T], R) ->
    fix_endlines(T, [H|R]).

char_lower(C) when C>=$A, C=<$Z ->
    C + 32;
char_lower(C) ->
    C.

char_upper(C) when C>=$a, C=<$z ->
    C - 32;
char_upper(C) ->
    C.

fix_char(C, #lisp_env{readtable_case=keep}) ->
    C;
fix_char(C, #lisp_env{readtable_case=upper}) ->
    char_upper(C);
fix_char(C, #lisp_env{readtable_case=lower}) ->
    char_lower(C).

		
    
