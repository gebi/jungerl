%%%----------------------------------------------------------------------
%%% File    : lersp_bif.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Simple BIFs. Not special forms - they're in the eval module.
%%% Created : 27 Nov 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(lersp_bif).
-author('luke@bluetail.com').

-export([cons/1, car/1, cdr/1, 'pair?'/1, list/1,
	 '+'/1, '-'/1, '=?'/1, '<'/1]).

-export([names/0]).

names() ->
    make_set([Fname || {Fname, _} <- module_info(exports)]) --
	non_bif_exports().

make_set([])    -> [];
make_set([X])   -> [X];
make_set([H|T]) -> [H | [X || X <- make_set(T), X /= H]].

non_bif_exports() ->
    [module_info, names].

%% ----------------------------------------------------------------------
%% Exported functions below are automatically BIFs.

cons([A,B]) -> [A|B].

car([[Car|_]]) -> Car.

cdr([[_|Cdr]]) -> Cdr.

'pair?'([[_|_]]) -> true;
'pair?'([_])     -> false.

list(L) -> L.

'+'([X|T]) -> X + '+'(T);
'+'([])    -> 0.

'-'([X])   -> -X;			% unary
'-'([X|T]) -> X - '+'(T);		% first minus sum of rest
'-'([])    -> 0.

'=?'([X,Y]) -> X == Y.

'<'([X,Y]) -> X < Y.

