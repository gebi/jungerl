%%%----------------------------------------------------------------------
%%% File    : edit_history.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Command history
%%% Created : 24 Mar 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_history).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").
-compile({parse_transform, edit_transform}).
-import(edit_lib, [buffer/1]).

-define(HISTORY_MAX_LENGTH, 500).

%% API
-export([bindings/2, add/2]).

%% Commands
-export([move/4, search/4]).

%% BaseVar = atom() name of edit_var variable to put history
%% RegionFn = fun(Buffer) -> {Start, End} of history region
%%
%% Returns a set of (standard) keybindings.
bindings(BaseVar, RegionFn) ->
    [{"M-p", {?MODULE, move,   [BaseVar, RegionFn, up]}},
     {"M-n", {?MODULE, move,   [BaseVar, RegionFn, down]}},
     {"M-r", {?MODULE, search, [BaseVar, RegionFn]}}
    ].

add(BaseVar, "") ->
    %% No blanks in history.
    skipped;
add(BaseVar, Item) ->
    Hist = edit_var:lookup(BaseVar, []),
    if
	hd(Hist) == Item ->
	    %% duplicate, leave history alone
	    ok;
	true ->
	    edit_var:set(BaseVar, trim_history([Item|Hist]))
    end.

move(State, BaseVar, RegionFn, Direction) ->
    IdxVar = index_var(BaseVar),
    HVar = list_var(BaseVar),
    History = ["" | edit_var:lookup(HVar, [])],
    Idx = case continuing_p(State) of
	      true ->
		  Prev = edit_var:lookup(IdxVar, 1),
		  case Direction of
		      up ->
			  Prev + 1;
		      down ->
			  Prev - 1
		  end;
	      false ->
		  2			% 1 is a new empty string
	  end,
    if
	Idx >= 1, length(History) >= Idx ->
	    Buf = buffer(State),
	    New = lists:nth(Idx, History),
	    kill_old(Buf, RegionFn),
	    edit_buf:insert(Buf, New, edit_buf:mark_pos(Buf, point)),
	    edit_var:set(IdxVar, Idx),
	    State;
	true ->
	    edit_util:status_msg(State, "No more history")
    end.

-command({search, [{string, "History regexp:"}]}).
search(State, BaseVar, RegionFn, Regexp) ->
    History = edit_var:lookup(BaseVar, []),
    case find_match(History, Regexp) of
	{match, Cmd} ->
	    Buf = buffer(State),
	    kill_old(Buf, RegionFn),
	    edit_buf:insert(Buf, Cmd, edit_buf:mark_pos(Buf, point)),
	    State;
	{error, Err} ->
	    edit_util:status_msg(State, "Error: " ++ regexp:format_error(Err));
	nomatch ->
	    edit_util:status_msg(State, "Not found")
    end.

continuing_p(State) ->
    case State#state.lastcmd of
	{?MODULE, move, _} ->
	    true;
	{?MODULE, search, _} ->
	    true;
	{_, history_move, _} ->
	    true;
	{_, history_search, _} ->
	    true;
	_ ->
	    false
    end.

kill_old(Buf, RegionFn) ->
    {Start, End} = RegionFn(Buf),
    edit_buf:delete(Buf, Start, End).

trim_history(Hist) when length(Hist) > ?HISTORY_MAX_LENGTH ->
    lists:sublist(Hist, ?HISTORY_MAX_LENGTH);
trim_history(Hist) ->
    Hist.

find_match([], Regexp) ->
    nomatch;
find_match([H|T], Regexp) ->
    case regexp:match(H, Regexp) of
	nomatch ->
	    find_match(T, Regexp);
	{match, _Start, _Length} ->
	    {match, H};
	{error, Err} ->
	    {error, Err}
    end.

list_var(Base)  -> Base.
index_var(Base) -> list_to_atom(atom_to_list(Base) ++ "_idx").

