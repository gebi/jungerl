%%%-------------------------------------------------------------------
%%% File    : bio.erl
%%% Author  : Mats Cronqvist <etxmacr@cbe2077>
%%% Description : block io
%%%
%%% Created : 13 Mar 2003 by Mats Cronqvist <etxmacr@cbe2077>
%%%-------------------------------------------------------------------
-module(bio).
-export([string/3,term/3]).

-define(BLOCK, 8092).

string(FN, Fun, Acc) ->
    Bfun = fun(_, O) -> {ok, lists:reverse(O)} end,
    in(FN, Fun, Acc, Bfun).
term(FN, Fun, Acc) ->
    Bfun = fun(C, O) -> to_term(C, lists:reverse([10|O])) end,
    in(FN, Fun, Acc, Bfun).

in(FN, Fun, Acc, Bfun) ->
    case file:open(FN, [read, raw]) of
        {ok, FD} -> 
            R = in(FD, file:read(FD, ?BLOCK), Fun, Bfun, {[], [], Acc}),
            file:close(FD),
            R;
        {error,R} -> exit({open_error, R, FN})
    end.
in(_FD, eof, _Fun, _Bfun, {_Cont, [], Acc}) -> Acc;
in(_FD, eof, Fun, Bfun, {Cont, O, Acc}) -> 
    case Bfun(Cont, O) of
        {ok, Term} -> Fun(Term, Acc);
        {cont, NCont} -> exit({incomplete_input, NCont})
    end;
in(FD, {ok, List}, Fun, Bfun, State) ->
    in(FD, file:read(FD, ?BLOCK), Fun, Bfun, do(List, Fun, Bfun, State)).

do([], _Fun, _Bfun, State) -> State;
do([13,10|R], Fun, Bfun, {Cont, O, Acc}) ->	%dos...
    do([10|R], Fun, Bfun, {Cont, O, Acc});
do([10|R], Fun, Bfun, {Cont, O, Acc}) ->
    case Bfun(Cont, O) of
        {cont, NCont} -> do(R, Fun, Bfun, {NCont, [], Acc});
        {ok, Term} -> do(R, Fun, Bfun, {[], [], Fun(Term, Acc)})
    end;
do([H|R], Fun, Bfun, {Cont, O, Acc}) ->
    do(R, Fun, Bfun, {Cont, [H|O], Acc}).

to_term(Cont, Str) ->
    case catch erl_scan:tokens(Cont, Str, 1) of
        {done, {ok, Toks, _}, []} -> 
            case catch erl_parse:parse_term(Toks) of
                {ok, Term} -> {ok, Term};
                {error, R} -> exit({parser_failed, R, Str})
            end;
        {more, Ncont} -> {cont, Ncont};
        _ -> exit({scanner_failed, Str})
    end.
