%%%----------------------------------------------------------------------
%%% File    : edit_file.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : file-related editor commands
%%% Created : 14 Jan 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_file).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").

-compile(export_all).
-compile({parse_transform, edit_transform}).

-import(edit_lib, [buffer/1]).

-command({find_file, [{file, "Filename:"}], "Open a file in a buffer."}).

mod_init() ->
    edit_var:set(auto_mode_alist, []),
    ok.

find_file(State, "") ->
    State;
find_file(State, Filename) ->
    BufferName = list_to_atom(lists:last(string:tokens(Filename, "/"))),
    NewState = find_file1(State, Filename, BufferName, 0),
    auto_set_mode(NewState).

%% Open Filename in the buffer Name. If Name exists and is visiting a
%% different file, then try Name<1>, etc. If we hit a buffer that's
%% already visiting this file, just attach to that.
find_file1(State, Filename, Name, N) ->
    BufferName = make_buffer_name(Name, N),
    case whereis(BufferName) of
	undefined ->
	    {ok, Buf} = edit_buf:new(BufferName),
	    edit_buf:set_filename(Buf, filename:absname(Filename)),
	    State1 = edit_util:set_buffer(State, BufferName),
	    case cord:new_from_file(Filename) of
		{ok, Cord} ->
		    edit_buf:set_text(Buf, Cord),
		    edit_buf:move_mark(Buf, point, 1),
		    State1;
		{error, Reason} ->
		    edit_util:status_msg(State1, "(New file)")
	    end;
	Pid ->
	    case edit_buf:get_filename(BufferName) of
		Filename ->
		    edit_util:set_buffer(State, BufferName);
		_ ->
		    find_file1(State, Filename, Name, N+1)
	    end
    end.

make_buffer_name(Atom, 0) ->
    Atom;
make_buffer_name(Atom, N) ->
    Name = io_lib:format("~s<~p>", [atom_to_list(Atom), N]),
    list_to_atom(lists:flatten(Name)).

save_file(State) ->
    case edit_buf:get_filename(buffer(State)) of
	undefined ->
	    edit_util:status_msg(State, "No file associated with buffer");
	Filename ->
	    Cord = edit_buf:get_cord(buffer(State)),
	    IO = cord:to_iolist(Cord),
	    ok = file:write_file(Filename, IO),
	    edit_util:status_msg(State, "Wrote ~s", [Filename])
    end.

auto_set_mode(State) ->
    AutoModeList = edit_var:lookup(auto_mode_alist),
    Buf = buffer(State),
    case edit_buf:get_filename(Buf) of
	undefined ->
	    State;
	Filename ->
	    auto_set_mode(State, Filename, AutoModeList)
    end.

auto_set_mode(State, Filename, []) ->
    State;
auto_set_mode(State, Filename, [{RE, {Mod, Fun}}|T]) ->
    case regexp:match(Filename, RE) of
	nomatch ->
	    auto_set_mode(State, Filename, T);
	{match, _, _} ->
	    Mod:Fun(State)
    end.


