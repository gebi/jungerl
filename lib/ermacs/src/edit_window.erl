%%%----------------------------------------------------------------------
%%% File    : edit_window.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Window handling functions
%%% Created : 14 Oct 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_window).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").

-compile(export_all).
%%-export([Function/Arity, ...]).

%% NB: Height is the total height including modeline
make_window(Buffer, Y, Width, Height) ->
    Id = make_ref(),
    W = #window{start_mark={start, Id},
		y=Y,
		width=Width,
		height=Height,
		id=Id},
    attach(W, Buffer).

%% Number of lines for viewing text - excludes modeline
text_lines(W) when W#window.minibuffer == true ->
    physical_lines(W);
text_lines(W) ->
    physical_lines(W) - 1.

physical_lines(W) ->
    W#window.height.

width(W) ->
    W#window.width.

%% "Attach" a window to a buffer. Puts a mark in the buffer so that
%% the window knows where it's up to.
attach(Window, Buffer) ->
    attach(Window, Buffer, 1).
attach(Window, Buffer, Start) ->
    edit_buf:add_mark(Buffer, Window#window.start_mark, 1, backward),
    Window#window{buffer=Buffer}.

