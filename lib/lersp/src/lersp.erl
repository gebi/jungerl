%%%-------------------------------------------------------------------
%%% File    : lersp.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : front-end interface
%%%
%%% Created : 27 Nov 2001 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(lersp).

-compile(export_all).

repl() ->
    repl(lersp_eval:state0()).

repl(S0) ->
    case catch print(eval(read(S0))) of
	{'EXIT', {error, Rsn}} ->
	    io:format("Error: ~s~n", [Rsn]),
	    repl(S0);
	{'EXIT', Rsn} ->
	    io:format("Crash: ~w~n", [Rsn]),
	    repl(S0);
	finished ->
	    ok;
	S1 ->
	    repl(S1)
    end.

read(S0) ->
    {io:get_line('lersp> '), S0}.

eval({eof, _}) ->
    throw(finished);
eval({Line, S0}) ->
    {Val,S1} = lersp_eval:eval_exprs(lersp_parse:parse_all(Line), S0).

print({Val, S1}) ->
    io:format("~s~n", [lersp_eval:pprint(Val)]),
    S1.

