%%%----------------------------------------------------------------------
%%% File    : edit_input.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Keyboard input server
%%% Created : 22 Jan 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_input).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").

-export([start_link/1, loop/1]).

%% Receiver will be sent {key_input, Char} each time a key is pressed.
start_link(Receiver) ->
    Pid = spawn_link(edit_input, loop, [Receiver]),
    register(?MODULE, Pid),
    Pid.

loop(Receiver) ->
    Ch = case ?EDIT_TERMINAL:read() of
	     $\n ->
		 $\r;
	     145 ->			% C-M-q is reserved for panic
		 panic();
	     X ->
		 X
	 end,
    Receiver ! {key_input, Ch},
    loop(Receiver).

panic() ->
    halt().
