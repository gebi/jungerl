%%%----------------------------------------------------------------------
%%% File    : em_scheme.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Scheme-mode
%%% Created : 30 Apr 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(em_scheme).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").
-import(edit_lib, [buffer/1]).

-compile(export_all).
%%-export([Function/Arity, ...]).

-define(keymap, scheme_mode_map).

-record(tok, {column,			% Column number
	      line,			% Line number
	      read_ahead,		% # chars read ahead
	      token			% Token returned from leex
	     }).

mod_init() ->
    catch edit_keymap:delete(?keymap),
    init_map(),
    edit_keymap:global_set_key("C-x s", {?MODULE, scheme_mode, []}),
    edit_var:add_to_list(auto_mode_alist,
			 {"\.scheme$$", {?MODULE, scheme_mode}}),
    ok.

init_map() ->
    edit_keymap:new(?keymap),
    edit_keymap:bind_each(?keymap, bindings()).

bindings() ->
    [{"C-i", {?MODULE, reindent_cmd, []}}
    ].

%% test buffer annotation
ann_trace(S0, Cord, Text, Start, End) ->
    io:format("trace: ~p at (~p,~p)~n", [Text, Start, End]),
    {ok, S0}.

scheme_mode(State) ->
    Mode = #mode{name="Scheme",
		 id=scheme,
		 keymaps=[?keymap]},
    Buf = buffer(State),
    Scanner = em_scan:make_scheme_scanner(),
    edit_buf:add_annotation(Buf, scan, {em_scan, scan_annotation, [Scanner]},
			    no_scan),
    %%edit_buf:add_annotation(Buf, scheme, {?MODULE, ann_trace, []}, []),
    edit_buf:set_mode(Buf, Mode),
    State.

scan_buffer(State) ->
    B = buffer(State),
    C = edit_buf:get_cord(B),
    Walker = cord:walker(C),
    Scanner = make_scheme_scanner(),
    case em_scan:edit_scan(Scanner, Walker) of
	{error, Rsn} ->
	    edit_util:status_msg(State, "Error: ~s",
				 [em_scheme_scan:format_error(Rsn)]);
	{ok, Toks} ->
	    edit_util:status_msg(State, "Scan: (~p) ~s",
				 [length(Toks), format_tokens(Toks)])
    end.

make_scheme_scanner() ->
    em_scan:make_scanner(em_scheme_scan:yystate(),
			 {em_scheme_scan, yystate},
			 {em_scheme_scan, yyaction}).

format_tokens([A,B|T]) ->
    format_token(A) ++ ", " ++ format_tokens([B|T]);
format_tokens([A]) ->
    format_token(A).

format_token({T, C, L}) ->
    io_lib:format("~p:~p:~p", [T, C, L]).

scan(W) ->
    em_scan:edit_scan(make_scheme_scanner(), W).

%%%----------------------------------------------------------------------
%%% Reindent

reindent_cmd(State) ->
    B = buffer(State),
    Start = beginning_of_fun_pos(B),
    End = edit_lib:beginning_of_line_pos(B),
    Region = edit_buf:get_region_cord(B, Start, End),
    Walker = cord:walker(Region),
    {ok, Scan} = scan(Walker),
    reindent(B, End, calc_indent(munge(Scan))).

munge([A = {_,_,LineA}, B = {_,_,LineB} | T]) when LineA /= LineB ->
    [strip(A), newline | munge([B|T])];
munge([H|T]) ->
    [strip(H) | munge(T)];
munge([]) ->
    [newline].

strip({Type, Col, Line}) -> {Type, Col}.

reindent(Buf, Pos, Lvl) ->
    Pred = fun(C) -> (C /= $ ) and (C /= $\t) end,
    End = max(Pos, edit_lib:find_char_forward(Buf, Pred, Pos, 1)),
    edit_buf:replace(Buf, lists:duplicate(Lvl, $ ), Pos, End).

beginning_of_fun_pos(B) ->
    Point = min(edit_buf:point_max(B) - 1,
		max(1, edit_lib:beginning_of_line_pos(B) - 1)),
    Cord = edit_buf:get_cord(B),
    case cord_regexp:first_match("^\\(", Cord, Point, backward) of
	nomatch ->
	    1;
	{match, Start, End} ->
	    Start
    end.

%% Calculate the indent level for the line *following* the tokens `Toks'.
calc_indent(Toks) ->
    io:format("calc_indent: ~p~n", [Toks]),
    calc_indent(Toks, 0, []).

%% ( s      : (lambda
%% push; |(|+2
calc_indent([{'(', OC},{special,SC}|T], Lvl0, S0) ->
    S1 = [Lvl0|S0],
    calc_indent(T, OC+2, S1);
%% ( a:!\n b:!\n
%% push; |b|
calc_indent([{'(',OC}, {_,_}, {_,C}|T], Lvl0, S0) ->
    S1 = [Lvl0|S0],
    calc_indent(T, C, S1);
calc_indent([{'(',OC}|T], Lvl0, S0) ->
    S1 = [Lvl0|S0],
    calc_indent(T, OC+1, S1);
calc_indent([{')',CC}|T], Lvl0, S0) ->
    [Lvl1|S1] = S0,
    calc_indent(T, Lvl1, S1);
calc_indent([H|T], Lvl, S) ->
    calc_indent(T, Lvl, S);
calc_indent([], Lvl, S) ->
    Lvl.

%% calc_indent(Tokens, CurrentLevel, State, Stack)
%% State = normal | special
%% Stack = [{State, Level}]


%% Opening

% calc_indent([{'(',COpen}, {special, CSpecial}|T], Lvl0, S0, Stk0) ->
%     %% opening a special form
%     calc_indent(T, COpen + 2, special, [{S0, Lvl0}|Stk0]);
% calc_indent([{'(',COpen}|T], Lvl0, S0, Stk0) ->
%     calc_indent(T, COpen+1, normal, [{S0, Lvl0}|Stk0]);
% calc_indent([{')', _}|T], Lvl0, S, [{SPrev,LvlPrev}|Stk]) ->
%     calc_indent(T, LvlPrev, SPrev, Stk);
% calc_indent([{')', _}|T], Lvl0, S, []) ->
%     %% Too many )'s - go back to 0
%     calc_indent(T, 0, normal, []);
% calc_indent([{Symbol, SC}, newline | T], Lvl0, normal, Stk0)
%   when Symbol == atom; Symbol == special ->
%     calc_indent(T, SC, normal0, Stk0);
% calc_indent([{atom, _}|T], Lvl0, S0, Stk0) ->
%     calc_indent(T, Lvl0, S0, Stk0);
% calc_indent([{special, _}|T], Lvl0, S0, Stk0) ->
%     calc_indent(T, Lvl0, S0, Stk0);
% calc_indent([newline|T], Lvl, S, Stk) ->
%     calc_indent(T, Lvl, S, Stk);
% calc_indent([], Lvl, S, Stk) ->
%     Lvl.

max(X, Y) when X > Y -> X;
max(X, Y)            -> Y.

min(X, Y) when X < Y -> X;
min(X, Y)            -> Y.
