%%%-------------------------------------------------------------------
%%% File    : lersp_parse.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : 
%%%
%%% Created : 27 Nov 2001 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(lersp_parse).

-import(lists, [reverse/1, member/2]).

-compile(export_all).

-define(OPEN, $().			% because emacs has trouble
-define(CLOSE, $)).
-define(WHITESPACE(X), X == $\s; X == $\r; X == $\n).

parse_all(S) ->
    case parse(S) of
	nothing ->
	    [];
	{sexp, {Sexp, Rest}} ->
	    [Sexp|parse_all(Rest)]
    end.

parse([]) ->
    nothing;
parse("'"++T) ->
    {sexp, {S, Rest}} = parse(T),
    {sexp, {[quote, S], Rest}};
parse("`"++T) ->
    {sexp, {S, Rest}} = parse(T),
    {sexp, {[quasiquote, S], Rest}};
parse(",@"++T) ->
    {sexp, {S, Rest}} = parse(T),
    {sexp, {['unquote-splicing', S], Rest}};
parse(","++T) ->
    {sexp, {S, Rest}} = parse(T),
    {sexp, {[unquote, S], Rest}};
parse([?OPEN|T]) ->
    parse_list(T, []);
parse([Ws|T]) when ?WHITESPACE(Ws) ->
    parse(T);
parse([C|T]) ->
    case is_numeral(C) of
	true ->
	    {sexp, parse_number([C|T], [])};
	false ->
	    case is_symchar(C) of
		true ->
		    {sexp, parse_symbol([C|T], [])};
		false ->
		    parse(T)
	    end
    end.

%% ----------------------------------------------------------------------

parse_number([], Acc) ->
    {make_number(Acc), []};
parse_number([C|T], Acc) ->
    case is_numeral(C) of
	true ->
	    parse_number(T, [C|Acc]);
	false ->
	    case is_symchar(C) of
		true ->
		    parse_symbol([C|T], Acc);
		false ->
		    {make_number(Acc), [C|T]}
	    end
    end.

make_number(Acc) ->
    list_to_integer(reverse(Acc)).

%% ----------------------------------------------------------------------

parse_symbol([], Acc) ->
    {make_symbol(Acc), []};
parse_symbol([C|T], Acc) ->
    case is_symchar(C) of
	true ->
	    parse_symbol(T, [C|Acc]);
	false ->
	    {make_symbol(Acc), [C|T]}
    end.

make_symbol(Acc) -> list_to_atom(reverse(Acc)).

%% ----------------------------------------------------------------------

parse_list([?CLOSE|T], Acc) ->
    {sexp, {dot(reverse(Acc)), T}};
parse_list(L, Acc) when L /= [] ->
    {sexp, {Sexp, Lrest}} = parse(L),
    parse_list(Lrest, [Sexp|Acc]).

dot(['.', X])            -> X;
dot([H|T]) when H /= '.' -> [H|dot(T)];
dot([])                  -> [].

%% ----------------------------------------------------------------------

is_symchar(C) when C >= $a, C =< $z -> true;
is_symchar(C) when C >= $A, C =< $Z -> true;
is_symchar(C) ->
    case is_numeral(C) of
	true ->
	    true;
	false ->
	    member(C, sym_symbols())
    end.

is_numeral(C) -> (C >= $0) and (C =< $9).

sym_symbols() ->
    "!@#$%^&*_+-=/?;:[]{}|,./`~".

