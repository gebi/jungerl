%%%-------------------------------------------------------------------
%%% File    : internal_exports.erl
%%% Purpose : Eliminate the need for export_all (parse transform)
%%%
%%% Created :  7 Mar 2005 by Luke Gorrie <luke@synap.se>
%%%-------------------------------------------------------------------
-module(internal_exports).

-export([parse_transform/2]).
-export([apply/3]).

%% This is a parse-transform module to help you call internal
%% functions when you need to (tm) without resorting to export_all.
%%
%% If you have a module like this:
%%
%%   -module(foo).
%%   -export([a/1]).
%%   -compile({parse_transform,internal_exports}).
%%   a(X)   -> foo:bar(X).
%%   b()    -> true.
%%   c(X,Y) -> pig:oink(X, Y).
%%
%% what we do is add on this:
%%
%%   'EXPORTER'() -> [{{a, 1}, fun a/1},
%%                    {{b, 0}, fun b/0},
%%                    {{c, 2}, fun c/2}]
%%
%% That way it's possible for other modules (i.e. the shell) to get a
%% hold of internal functions that aren't exported in the usual sense.

-define(EXPORTER, 'EXPORTER').

%% We use a constant line number because the generated code will
%% always be correct. </straightface>
-define(LINE0, 0).

%% Like erlang:apply/3 but using the EXPORTER function.
apply(Module, Function, Args) ->
    case lists:keysearch({Function, length(Args)}, 1, Module:?EXPORTER()) of
	{value, {_, Fun}} ->
	    erlang:apply(Fun, Args);
	false ->
	    erlang:fault({undefined, Module, Function, length(Args)})
    end.

parse_transform(ParseTree, Options) ->
    case functions(ParseTree) of
	[] ->
	    %% No functions in this module. Leave it alone.
	    ParseTree;
	Fs ->
	    P1 = add_export(ParseTree, ?EXPORTER, 0),
	    F = exporter_function(Fs),
	    insert_function(P1, F)
    end.

%% add_export(ParseTree, Function, Arity) -> ParseTree'
add_export([{attribute, Line, export, Es}|T], Function, Arity) ->
    [{attribute, Line, export, Es ++ [{Function, Arity}]} | T];
add_export(P = [{function, _, _, _, _}|_], Function, Arity) ->
    %% There was no export list. Create one.
    [{attribute, 0, export, [{Function, Arity}]} | P];
add_export([H|T], Function, Arity) ->
    [H|add_export(T, Function, Arity)].

%% Return [{Name, Arity}] of all functions in the module.
functions(P) ->
    [{Name, Arity} || {function, _, Name, Arity, _} <- P].

%% insert_function(P, F) -> P'
%% Insert a new function at the end of the parse tree.
insert_function([{eof, Line}], F) ->
    [F, {eof, Line}];
insert_function([H|T], F) ->
    [H|insert_function(T, F)].

%% Return the definition of the 'EXPORTER' function.
exporter_function(Fs) ->
    Line = ?LINE0,
    {function, Line, ?EXPORTER, Line,
     [{clause, Line, [], [], [gen_funlist(Line, Fs)]}]}.

%% gen_funlist([{Fun, Arity}]) -> code for [{{Fun,Arity},fun Fun/Arity}]
gen_funlist(Line, []) ->
    {nil, Line};
gen_funlist(Line, [{Name,Arity}|T]) ->
    {cons, Line,
     {tuple, Line, [{tuple, Line, [{atom, Line, Name},{integer, Line, Arity}]},
		    {'fun', Line, {function, Name, Arity}}]},
     gen_funlist(Line, T)}.

