%%%----------------------------------------------------------------------
%%% File    : edit_transform.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Parse transform module for editor command modules
%%% Created : 25 Oct 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

% -command({cmd1, [{name1, prompt1}], doc1}).
% -command({cmd2, []}).

-module(edit_transform).
-author('luke@bluetail.com').

-export([parse_transform/2]).

%% Collect "command" attributes to generate a command_info function. These
%% like (interactive ...) declarations in ELisp. See edit_lib for examples.
%%
%% command grammar:
%% -command({Fun, [Arg]}) | -command({Fun, [Arg], DocString})
%% Arg = {TypeAtom, PromptString}
%%
%% command_info() returns [{Fun, [Arg], DocString}]
%%
parse_transform(Form, Opts) ->
    {Head, Rest} = split(Form),
    {Body, EOF} = splitlast(Rest),
    Line = element(2, hd(Body)),
    {Cmds, NewBody} = separate(Body),
    ListSrc = lists:flatten(io_lib:format("~p", [Cmds])),
    Export = scan_and_parse("-export([command_info/0]).", Line),
    Fun = scan_and_parse("command_info() -> " ++ ListSrc ++ ".", Line),
    NewForm = Head ++ [Export] ++ NewBody ++ [Fun, EOF],
    NewForm.

%% => {Commands, FilteredBody}
%%    Commands = {Name, Params, DocString}
separate(Body) ->
    separate(Body, [], []).

separate([{attribute, _Line, command, {Name, Params, Doc}}|T], Acc1, Acc2) ->
    separate(T, [{Name, Params, Doc}|Acc1], Acc2);
separate([{attribute, _Line, command, {Name, Params}}|T], Acc1, Acc2) ->
    separate(T, [{Name, Params, ""}|Acc1], Acc2);
separate([H|T], Acc1, Acc2) ->
    separate(T, Acc1, [H|Acc2]);
separate([], Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)}.

scan_and_parse(Source, Line) ->
    {ok, FunTokens, _} = erl_scan:string(Source, Line),
    {ok, FunParse} = erl_parse:parse_form(FunTokens),
    FunParse.

%% Split into {Head, Body}. Head is everything upto and including the
%% 'module' attribute, Body is the rest.
split(Form) ->
    split(Form, []).
split([X = {attribute, Line, module, Module}|T], Acc) ->
    {lists:reverse([X|Acc]), T};
split([H|T], Acc) ->
    split(T, [H|Acc]).

splitlast(L) ->
    splitlast(L, []).
splitlast([H], Acc)   -> {lists:reverse(Acc), H};
splitlast([H|T], Acc) -> splitlast(T, [H|Acc]).


