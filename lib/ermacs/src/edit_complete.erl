%%%----------------------------------------------------------------------
%%% File    : edit_complete.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Minibuffer completion.
%%% Created : 26 Mar 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_complete).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).
%%-export([Function/Arity, ...]).

-import(edit_lib, [buffer/1]).

-define(completions_buffer, '*Completions*').

%% Variables:
%%   completion_state     = initial | ambiguous | showing | nomatch
%%   completion_type      = file | boring_atom()
%%   completion_prev_path = string()
completion_init(Type) ->
    edit_var:set(completion_state, fresh),
    edit_var:set(completion_prev_path, ""),
    edit_var:set(completion_type, Type),
    ok.

complete(State) ->
    complete(State, edit_var:lookup(completion_type)).

complete(State, file) ->
    MBuf = buffer(State),
    Path = edit_buf:get_text(MBuf),
    FSMState = case edit_var:lookup(completion_prev_path) of
		   Path ->
		       edit_var:lookup(completion_state);
		   _ ->
		       %% Different path, reset fsm
		       initial
	       end,
    Result = filename_complete(Path),
    {State1, NewFSMState, NewPath} =
	do_complete(State, FSMState, Path, Result),
    edit_buf:set_text(MBuf, NewPath),
    edit_var:set(completion_state, NewFSMState),
    edit_var:set(completion_prev_path, NewPath),
    State1;
complete(State, _) ->
    State.

do_complete(EditState, FSMState, Path, Result) ->
    {NextFSMState, Action} = complete_fsm(FSMState, Result),
    {EditState1, NewPath} = action(EditState, Path, Action),
    {EditState1, NextFSMState, NewPath}.

%% complete_fsm(State, CompletionResult) => {NextState, Action}
%%
%% State = initial | ambiguous | showing | nomatch
%% CompletionResult = {unique, Path} | {completions, Path, List} | nomatch
%% Action = nop | scroll | {show, List} | {rewrite, Path}

%% FIXME: Add transitions to allow for files being created/deleted
%% between inputs.

complete_fsm(initial, {unique, New}) ->
    {initial, {rewrite, New}};
complete_fsm(initial, {completions, New, List}) ->
    {ambiguous, {rewrite, New}};
complete_fsm(initial, nomatch) ->
    {nomatch, nop};

complete_fsm(ambiguous, {completions, New, List}) ->
    {showing, {show, List}};

complete_fsm(showing, {completions, New, List}) ->
    {showing, scroll};

complete_fsm(nomatch, nomatch) ->
    {nomatch, nop};

%% catch all for unexpected cases
complete_fsm(_, _) ->
    {nomatch, nop}.

%% action(State, Path, Action) => {State', Path'}
action(State, Path, nop) ->
    {State, Path};
action(State, Path, scroll) ->
    {edit_util:window_map(State, fun maybe_complete_scroll/1), Path};
action(State, Path, {show, List}) ->
    {NewState, Buf} = make_completion_buffer(State, List),
    {edit_util:popup_buffer(NewState, Buf), Path};
action(State, Path, {rewrite, NewPath}) ->
    {State, NewPath}.
    
make_completion_buffer(St, AbsStrings) ->
    BaseStrings = lists:map(fun(X) -> filename:basename(X) end,
			    AbsStrings),
    Strings = lists:sort(BaseStrings),
    Name = ?completions_buffer,
    edit_buf:new(Name),
    edit_buf:set_text(Name, ""),
    lists:foreach(fun(String) ->
			  P = edit_buf:mark_pos(Name, point),
			  edit_buf:insert(Name, String ++ "\n", P)
		  end,
		  Strings),
    edit_buf:move_mark(Name, point, 1),
    St1 = case lists:member(Name, St#state.buffers) of
	      true ->
		  St;
	      false ->
		  St#state{buffers=[Name|St#state.buffers]}
	  end,
    {St1, Name}.

maybe_complete_scroll(Win) when Win#window.buffer == ?completions_buffer ->
    edit_lib:scroll_down_wrap(Win);
maybe_complete_scroll(Win) ->
    Win.

%% Returns: {unique, Complete}
%%        | {completions, LongestPrefix, [string()]}
%%        | nomatch
filename_complete(Path) ->
    Dir = filename:dirname(Path),
    case file:list_dir(Dir) of
	{ok, BaseNames} ->
	    Names1 = lists:map(fun(X) -> filename:join(Dir, X) end,
			      BaseNames),
	    Names2 = lists:map(fun dirify/1, Names1),
	    string_complete(Path, Names2);
	Err ->
	    nomatch
    end.

%% Append a "/" to directory names if they don't already have them.
dirify(Path) ->
    case file:read_file_info(Path) of
	{ok, Inf} when Inf#file_info.type == directory ->
	    case lists:last(Path) of
		$/ ->
		    Path;
		_ ->
		    Path ++ "/"
	    end;
	_ ->
	    Path
    end.

string_complete(In, L) ->
    case lists:filter(fun(X) -> lists:prefix(In, X) end, L) of
	[C] ->
	    {unique, C};
	[] ->
	    nomatch;
	Completions ->
	    {completions, longest_prefix(Completions), Completions}
    end.

longest_prefix([])         -> [];
longest_prefix([A, B | T]) -> longest_prefix([longest_prefix1(A, B) | T]);
longest_prefix([X])        -> X.

longest_prefix1([X|T1], [X|T2]) -> [X|longest_prefix1(T1, T2)];
longest_prefix1(_, _)           -> [].

