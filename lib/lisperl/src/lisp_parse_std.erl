%%%-------------------------------------------------------------------
%%% File    : lisp_parse_std.erl
%%% Author  : Vlad Dumitrescu <vlad_dumitrescu@hotmail.com>
%%% Description : A CommonLisp reader environment. To be used with lisp_parse.
%%%
%%% Created : 30 Mar 2004 by Vlad Dumitrescu <vlad_dumitrescu@hotmail.com>
%%%
%%% To do:
%%%    - implement more # macros
%%%    - #\ is wrong
%%%
%%% $Id$
%%%
%%%-------------------------------------------------------------------

-module(lisp_parse_std).

-export([
	 standard_env/0,

	 test/0,
	 test/1,
	 test_all/0
	]).

%-compile(export_all).

%%% standard macros

parse_list(L, C, N, Env) ->
    parse_list(L, C, N, Env, []).

parse_list(L, C, N, Env, Result) ->
    case catch lisp_parse:parse(L, N, Env) of
	{'EXIT', Reason} ->
	    throw({error, {unexpected_error, Reason}, N, L});
	nothing ->
	    throw({error, unexpected_eof, N, L});
	{nothing, Rest} ->
	    parse_list(Rest, C, N, Env, Result);
	{error, 'misplaced_)', Nx, Rest} ->
	    {sexp, {dot(lists:reverse(Result)), N}, Rest};
	{error, Reason, Nx, Rest} ->
	    {error, Reason, Nx, Rest};
	{sexp, {Sexp, N1}, Lrest} ->
	    parse_list(Lrest, C, N1, Env, [{Sexp, N1}|Result])
    end.

dot([{'.', _}, X]) -> 
    X;
dot([{'.', N} | X]) -> 
    throw({error, misplaced_dot_in_list, N, []});
dot([E|T]) -> 
    [E|dot(T)];
dot([]) -> 
    [].

end_parse_list(L, C, N, Env) ->
    throw({error, 'misplaced_)', N, L}).

parse_comment([$\n|T]=L, C, N, Env) ->
    lisp_parse:parse(L, N, Env);
parse_comment([Ch|L], C, N, Env) ->
    parse_comment(L, C, N, Env).

parse_quote(L, C, N, Env) ->
    case lisp_parse:parse(L, N, Env) of
	nothing ->
	    throw({error, unexpected_eof, N, L});
	{sexp, {Sexp, N1}, Rest} ->
	    {sexp, {[{quote, N}, {Sexp, N1}], N1}, Rest}
    end.

parse_string(L, C, N, Env) ->
    parse_string(L, C, N, Env, []).

parse_string([], C, N, Env, Result) ->
    throw({error, unexpected_eof, N, []});
parse_string([C|L], C, N, Env, Result) ->
    {sexp, {lists:reverse(Result), N}, L};
parse_string([Ch|L], C, N, Env, Result) ->
    Type = lisp_parse:get_syntax_type(Ch, Env),
    {C1, L1} = case Type of
		   single_escape ->
		       [Cc|L0] = L,
		       {Cc, L0};
		   _ ->
		       {Ch, L}
	       end,
    N1 = if Ch == $\n -> N + 1;
	    true -> N
	 end,
    parse_string(L1, C, N1, Env, [C1|Result]).

'parse_macro_('(L, Arg, $(, N, Env) ->
    {sexp, {Sexp, Nn}, Rest} = parse_list(L, $(, N, Env),
    {sexp, {list_to_tuple(Sexp), Nn}, Rest}.

'parse_macro_|'([$|, $# |L], Arg, $|, N, Env) ->
    lisp_parse:parse(L, N, Env);
'parse_macro_|'([$\n|L], Arg, $|, N, Env) ->
    'parse_macro_|'(L, Arg, $|, N+1, Env);
'parse_macro_|'([Ch|L], Arg, $|, N, Env) ->
    Type = lisp_parse:get_syntax_type(Ch, Env),
    {C1, L1} = case Type of
		   single_escape ->
		       [Cc|L0] = L,
		       {Cc, L0};
		   _ ->
		       {Ch, L}
	       end,
    N1 = if Ch == $\n -> N + 1;
	    true -> N
	 end,
    'parse_macro_|'(L1, Arg, $|, N1, Env).
    
'parse_macro_\\'([Ch|L], Arg, $\\, N, Env) ->
    %% BUG: should read until a whitespace?
    %%    erlang:display({dispatch, "\\", lists:sublist(L, 20)}),
    {sexp, {Ch, N}, L}.

standard_env() ->
    Env = lisp_parse:set_readtable_case(lisp_parse:new_env(), keep),
    Env0 = lisp_parse:def_chars([
		 {  0, whitespace},
		 {  1, whitespace},
		 {  2, whitespace},
		 {  3, whitespace},
		 {  4, whitespace},
		 {  5, whitespace},
		 {  6, whitespace},
		 {  7, whitespace},
		 {  8, constituent}, % backspace
		 {  9, whitespace}, % tab
		 { 10, whitespace}, % lf
		 { 11, whitespace},
		 { 12, whitespace}, % page
		 { 13, whitespace}, % cr
		 { 14, whitespace},
		 { 15, whitespace},
		 { 16, whitespace},
		 { 17, whitespace},
		 { 18, whitespace},
		 { 19, whitespace},
		 { 20, whitespace},
		 { 21, whitespace},
		 { 22, whitespace},
		 { 23, whitespace},
		 { 24, whitespace},
		 { 25, whitespace},
		 { 26, whitespace},
		 { 27, whitespace},
		 { 28, whitespace},
		 { 29, whitespace},
		 { 30, whitespace},
		 { 31, whitespace},
		 { 32, whitespace}, % space
		 { 33, constituent}, %!
		 { 34, macro, terminating}, % "
		 { 35, macro, terminating, dispatching}, % #
		 { 36, constituent}, % $
		 { 37, constituent}, % %
		 { 38, constituent}, % &
		 { 39, macro, terminating}, % '
		 { 40, macro, terminating}, % (
		 { 41, macro, terminating}, % )
		 { 42, constituent}, % *
		 { 43, constituent}, % +
		 { 44, macro, terminating}, % ,
		 { 45, constituent}, % -
		 { 46, constituent}, % .
		 { 47, constituent}, % /
		 { 48, constituent}, % 0
		 { 49, constituent}, %
		 { 50, constituent}, %
		 { 51, constituent}, %
		 { 52, constituent}, %
		 { 53, constituent}, %
		 { 54, constituent}, %
		 { 55, constituent}, %
		 { 56, constituent}, %
		 { 57, constituent}, % 9
		 { 58, constituent}, % :
		 { 59, macro, terminating}, % ;
		 { 60, constituent}, % <
		 { 61, constituent}, % =
		 { 62, constituent}, % >
		 { 63, constituent}, % ? 
		 { 64, constituent}, % @
		 { 65, constituent}, % A
		 { 66, constituent}, %
		 { 67, constituent}, %
		 { 68, constituent}, %
		 { 69, constituent}, %
		 { 70, constituent}, %
		 { 71, constituent}, %
		 { 72, constituent}, %
		 { 73, constituent}, %
		 { 74, constituent}, %
		 { 75, constituent}, %
		 { 76, constituent}, %
		 { 77, constituent}, %
		 { 78, constituent}, %
		 { 79, constituent}, %
		 { 80, constituent}, %
		 { 81, constituent}, %
		 { 82, constituent}, %
		 { 83, constituent}, %
		 { 84, constituent}, %
		 { 85, constituent}, %
		 { 86, constituent}, %
		 { 87, constituent}, %
		 { 88, constituent}, %
		 { 89, constituent}, %
		 { 90, constituent}, % Z
		 { 91, constituent}, % [
		 { 92, single_escape}, % \
		 { 93, constituent}, % ]
		 { 94, constituent}, % ^
		 { 95, constituent}, % _
		 { 96, macro, terminating}, % `
		 { 97, constituent}, % a
		 { 98, constituent}, %
		 { 99, constituent}, %
		 {100, constituent}, %
		 {101, constituent}, %
		 {102, constituent}, %
		 {103, constituent}, %
		 {104, constituent}, %
		 {105, constituent}, %
		 {106, constituent}, %
		 {107, constituent}, %
		 {108, constituent}, %
		 {109, constituent}, %
		 {110, constituent}, %
		 {111, constituent}, %
		 {112, constituent}, %
		 {113, constituent}, %
		 {114, constituent}, %
		 {115, constituent}, %
		 {116, constituent}, %
		 {117, constituent}, %
		 {118, constituent}, %
		 {119, constituent}, %
		 {120, constituent}, %
		 {121, constituent}, %
		 {122, constituent}, % z
		 {123, constituent}, % {
		 {124, multiple_escape}, % |
		 {125, constituent}, % }
		 {126, constituent}, % ~
		 {127, constituent} % 127
	      ], Env),

    Env1 = lisp_parse:def_macro_chars([{$(, fun parse_list/4},
				       {$), fun end_parse_list/4},
				       {$', fun parse_quote/4}, 
				       {$", fun parse_string/4}, 
				       {$;, fun parse_comment/4}
				      ], Env0),
    Env2 = lisp_parse:def_dispatch_chars([{$\\, fun 'parse_macro_\\'/5},
					  {$(, fun 'parse_macro_('/5},
					  {$|, fun 'parse_macro_|'/5}
					 ], Env1),
    Env2.

test(S) ->
    Env = standard_env(),

    io:format("~s~n", [S]),
    X = lisp_parse:parse_all(S, 1, Env),
    io:format("~p~n", [X]),
    Y = lisp_parse:strip_lines(X),
    io:format("~p~n", [Y]),
    io:format("~n"),
    ok.


test() ->
    Env = standard_env(),
    X = lisp_parse:parse_file("test.lisp", Env),
    io:format("~p~n", [X]),
    io:format("~n"),
    Y = lisp_parse:strip_lines(X),
    io:format("~p~n", [Y]),
    io:format("~n"),
    ok.

test_all() ->
    Env = standard_env(),
    {ok, Files} = file:list_dir("."),
    ElFiles = lists:filter(fun(X) ->
				   "psl." == lists:sublist(lists:reverse(X), 4)
			   end,
			   Files),
    lists:foreach(fun(X) ->
			  case catch lisp_parse:parse_file(X, Env) of
			      {'EXIT', Reason} ->
				  io:format("error in ~p~n~p~n~n", [X, X]);
			      _ ->
				  ok
			  end
		  end,
		  ElFiles),
    ok.
    
