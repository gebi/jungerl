%%%----------------------------------------------------------------------
%%% File    : edit_terminal.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : ncurses terminal implementation
%%% Created : 16 Sep 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_terminal).
-author('luke@bluetail.com').

-include_lib("slang/include/slang.hrl").

-define(ESC, 27).

-compile(export_all).
%%-export([Function/Arity, ...]).

setup() ->
    slang:tt_get_terminfo(),
    slang:kp_init(),
    slang:init_tty(0, 1, 1),
    slang:set_abort_signal(null),
    slang:smg_init_smg (),
    slang:smg_normal_video(),
    slang:setvar(newline_behaviour, ?NEWLINE_MOVES),
    refresh(),
    ok.

teardown() ->
    slang:smg_reset_smg(),
    slang:reset_tty(),
    ok.

newline() ->
    put_char($\n).

put_char(C) ->
    slang:smg_write_char(C).

put_string(S) ->
    slang:smg_write_string(S).

format(Fmt, Args) ->
    slang:smg_printf(Fmt, Args).

erase_to_eol() ->
    slang:smg_erase_eol().

move_to(X, Y) ->
    slang:smg_gotorc(Y, X).

refresh() ->
    slang:smg_refresh().

invalidate() ->
    slang:smg_touch_screen().

width() ->
    slang:getvar(screen_cols).

height() ->
    slang:getvar(screen_rows).

read() ->
    case slang:getkey() of
	?ESC ->
	    read() bor 2#10000000;
	N ->
	    N
    end.

font_reverse() ->
    slang:smg_reverse_video().

font_normal() ->
    slang:smg_normal_video().

