%%%----------------------------------------------------------------------
%%% File    : edit_help.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Help-related functions
%%% Created :  2 Feb 2002 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_help).

-include_lib("ermacs/include/edit.hrl").

-import(edit_lib, [buffer/1]).

-compile(export_all).
-compile({parse_transform, edit_transform}).

describe_key(S) ->
    edit_util:popup_message(S, '*Help*', description(lookup_key(S))).

description({Mod, Fun, Args}) ->
    case edit_extended:find_cmd_info(Mod, Fun) of
	{[], ""} ->
	    io_lib:format("Command: ~s:~s", [Mod, Fun]);
	{Params, Doc} ->
	    io_lib:format("Command: ~s:~s ~s\n~s",
			  [Mod,
			   Fun,
			   [io_lib:format("~p ", [Param]) ||
			       {Param,_} <- Params],
			   Doc])
    end;
description(X) ->
    lists:flatten(io_lib:format("~p", [X])).

find_source(S) ->
    case lookup_key(S) of
	{Mod, Fun, Args} ->
	    find_source(S, Mod, Fun);
	_ ->
	    edit_lib:status_msg(S, "Not bound to a function")
    end.

find_source(S0, Mod, Fun) ->
    case guess_source_file(code:which(Mod)) of
	{ok, Filename} ->
	    S1 = edit_file:find_file(S0, Filename),
	    Buf = buffer(S1),
	    edit_buf:move_mark(Buf, point, 1),
	    edit_lib:regexp_search(S1, forward, [$\n]++atom_to_list(Fun)),
	    edit_lib:beginning_of_line(S1)
    end.

guess_source_file(S0) ->
    case regexp:sub(S0, "ebin", "src") of
	{ok, S1, _} ->
	    case regexp:sub(S1, "beam", "erl") of
		{ok, S2, _} ->
		    case file:read_file_info(S2) of
			{ok, _} ->
			    {ok, S2};
			_ ->
			    error
		    end;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

%% ----------------------------------------------------------------------
%% Keymap lookup - cut&paste from edit.erl

lookup_key(State) ->
    io:format("here.~n"),
    Buf = buffer(State),
    Keymaps = (edit_buf:get_mode(Buf))#mode.keymaps ++ [global_map],
    lookup_key(State, Keymaps).

lookup_key(State, Keymaps) ->
    Ch = edit:get_key(),
    io:format("Snarfed key: ~p ~p~n", [Ch, Keymaps]),
    lookup_key(State, Keymaps, Ch).

lookup_key(State, [], Ch) ->
    unbound;
lookup_key(State, [Keymap|Keymaps], Ch) ->
    case edit_keymap:lookup(Keymap, Ch) of
	{ok, {keymap, NewMap}} ->
	    io:format("Retrying with ~p~n", [NewMap]),
	    lookup_key(State, [NewMap]);
	{ok, Cmd} ->
	    Cmd;
	unbound ->
	    lookup_key(State, Keymaps, Ch)
    end.

